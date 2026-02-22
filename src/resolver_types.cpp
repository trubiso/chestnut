#include "levenshtein.hpp"
#include "resolver.hpp"

#include <algorithm>
#include <cstdlib>
#include <iostream>
#include <sstream>
#include <variant>

Resolver::TypeInfo Resolver::from_type(AST::Type::Atom const& atom, FileContext::ID file_id, bool partial) {
	switch (atom.kind()) {
	case AST::Type::Atom::Kind::Float:
		return TypeInfo::make_known_float(TypeInfo::KnownFloat {atom.get_float().width});
	case AST::Type::Atom::Kind::Void:     return TypeInfo::make_known_void();
	case AST::Type::Atom::Kind::Char:     return TypeInfo::make_known_char();
	case AST::Type::Atom::Kind::Bool:     return TypeInfo::make_known_bool();
	case AST::Type::Atom::Kind::Named:    break;
	case AST::Type::Atom::Kind::Inferred: return TypeInfo::make_unknown();
	case AST::Type::Atom::Kind::Integer:  break;
	}

	if (atom.is_named()) {
		auto const& named = atom.get_named();

		// for named types, we need to extract the generic list first
		std::vector<std::tuple<std::optional<std::string>, TypeInfo::ID>> generics {};
		if (named.generic_list.has_value()) {
			for (auto const& generic : named.generic_list.value().ordered) {
				TypeInfo     type    = from_type(generic.value, file_id, partial);
				TypeInfo::ID type_id = register_type(std::move(type), generic.span, file_id);
				generics.emplace_back(std::nullopt, type_id);
			}
			for (auto const& [label, generic] : named.generic_list.value().labeled) {
				TypeInfo     type    = from_type(generic.value, file_id, partial);
				TypeInfo::ID type_id = register_type(std::move(type), generic.span, file_id);
				generics.emplace_back(label.value, type_id);
			}
		}

		// then point to the identifier if partial, get the ids otherwise
		return partial ? TypeInfo::make_named(&named.name.value, std::move(generics))
		               : TypeInfo::make_named(named.name.value.id.value(), std::move(generics));
	}

	// for integers, we need to determine how much information we know
	AST::Type::Atom::Integer const& integer = atom.get_integer();
	if (integer.width_type() != AST::Type::Atom::Integer::WidthType::Any)
		return TypeInfo::make_known_integer(TypeInfo::KnownInteger {integer});
	else return TypeInfo::make_partial_integer(TypeInfo::PartialInteger {integer, true});
}

Resolver::TypeInfo Resolver::from_type(AST::Type::Pointer const& pointer, FileContext::ID file_id, bool partial) {
	TypeInfo     inner   = from_type(pointer.type->value, file_id, partial);
	TypeInfo::ID pointee = register_type(std::move(inner), pointer.type->span, file_id);
	return TypeInfo::make_pointer(TypeInfo::Pointer {pointee, pointer.mutable_});
}

Resolver::TypeInfo Resolver::from_type(AST::Type const& type, FileContext::ID file_id, bool partial) {
	switch (type.kind()) {
	case AST::Type::Kind::Atom:    return from_type(type.get_atom(), file_id, partial);
	case AST::Type::Kind::Pointer: return from_type(type.get_pointer(), file_id, partial);
	}
	[[assume(false)]];
}

bool Resolver::TypeInfo::is_callable(std::vector<TypeInfo> const& pool) const {
	switch (kind()) {
	case Kind::Function: return true;
	case Kind::SameAs:
		for (TypeInfo::ID id : get_same_as().ids)
			if (pool.at(id).is_callable(pool)) return true;
		return false;
	default: return false;
	}
}

bool Resolver::TypeInfo::is_pointer(std::vector<TypeInfo> const& pool) const {
	switch (kind()) {
	case Kind::Pointer: return true;
	case Kind::SameAs:
		// TODO: maybe deal with ambiguous derefs
		if (get_same_as().ids.size() != 1) return false;
		return pool.at(get_same_as().ids.at(0)).is_pointer(pool);
	default: return false;
	}
}

std::vector<Resolver::TypeInfo::ID>
Resolver::TypeInfo::get_callable_subitems(TypeInfo::ID self_id, std::vector<TypeInfo> const& pool) const {
	switch (kind()) {
	case Kind::Function: return {self_id};
	case Kind::SameAs:   break;
	default:             return {};
	}

	// for SameAs, we just collect all the children
	std::vector<ID> ids {};
	for (TypeInfo::ID id : get_same_as().ids) {
		std::vector<ID> subids = pool.at(id).get_callable_subitems(id, pool);
		std::copy(subids.cbegin(), subids.cend(), std::back_inserter(ids));
	}

	return ids;
}

Resolver::TypeInfo::ID Resolver::TypeInfo::get_pointee(std::vector<TypeInfo> const& pool) const {
	assert(is_pointer(pool));
	switch (kind()) {
	case Kind::Pointer: return get_pointer().pointee;
	case Kind::SameAs:  return pool.at(get_same_as().ids.at(0)).get_pointee(pool);
	default:            [[assume(false)]]; return {};
	}
}

bool Resolver::TypeInfo::get_pointer_mutable(std::vector<TypeInfo> const& pool) const {
	assert(is_pointer(pool));
	switch (kind()) {
	case Kind::Pointer: return get_pointer().mutable_;
	case Kind::SameAs:  return pool.at(get_same_as().ids.at(0)).get_pointer_mutable(pool);
	default:            [[assume(false)]]; return {};
	}
}

std::optional<Resolver::TypeInfo const*>
Resolver::TypeInfo::get_single_underlying(std::vector<TypeInfo> const& pool) const {
	if (is_same_as()) {
		auto const& ids = get_same_as().ids;
		if (ids.size() != 1) return {};
		return pool.at(ids.at(0)).get_single_underlying(pool);
	}

	return this;
}

void Resolver::debug_print_type(TypeInfo::ID id) const {
	std::cout << "$" << id << " ";
	debug_print_type(type_pool_.at(id));
}

void Resolver::debug_print_type(TypeInfo type) const {
	switch (type.kind()) {
	case TypeInfo::Kind::Unknown:        std::cout << "(unknown type)"; return;
	case TypeInfo::Kind::Bottom:         std::cout << "(bottom)"; return;
	case TypeInfo::Kind::Module:         std::cout << "(module)"; return;
	case TypeInfo::Kind::KnownVoid:      std::cout << "void"; return;
	case TypeInfo::Kind::KnownChar:      std::cout << "char"; return;
	case TypeInfo::Kind::KnownBool:      std::cout << "bool"; return;
	case TypeInfo::Kind::PartialFloat:   std::cout << "(float)"; return;
	case TypeInfo::Kind::Function:
	case TypeInfo::Kind::SameAs:
	case TypeInfo::Kind::Generic:
	case TypeInfo::Kind::MemberAccess:
	case TypeInfo::Kind::Named:
	case TypeInfo::Kind::Pointer:
	case TypeInfo::Kind::KnownInteger:
	case TypeInfo::Kind::KnownFloat:
	case TypeInfo::Kind::PartialInteger: break;
	}

	if (type.is_function()) {
		TypeInfo::Function const& function = type.get_function();
		std::cout << "(function with args (";
		size_t count = 0;
		for (auto const& [name, arg_type] : function.arguments) {
			std::cout << (name.has_value() ? name.value() : "(anonymous)") << ": ";
			debug_print_type(arg_type);
			if (++count < function.arguments.size()) std::cout << ", ";
		}
		std::cout << ") and return type ";
		debug_print_type(function.return_);
		std::cout << ")";
	} else if (type.is_same_as()) {
		std::cout << "=(";
		size_t count = 0;
		for (TypeInfo::ID subid : type.get_same_as().ids) {
			debug_print_type(subid);
			if (++count < type.get_same_as().ids.size()) std::cout << " | ";
		}
		std::cout << ")";
	} else if (type.is_generic()) {
		TypeInfo::Generic const& generic = type.get_generic();
		std::cout << symbol_pool_.at(generic.name).name;
		if (!generic.declared_constraints.empty()) {
			std::cout << ": ";
			size_t count = 0;
			for (auto const& constraint : generic.declared_constraints) {
				std::cout << symbol_pool_.at(constraint.name).name;
				if (!constraint.arguments.empty()) {
					std::cout << '<';
					size_t subcount = 0;
					for (TypeInfo::ID subtype : constraint.arguments) {
						debug_print_type(subtype);
						if (++subcount < constraint.arguments.size()) std::cout << ", ";
					}
					std::cout << '>';
				}
				if (++count < generic.declared_constraints.size()) std::cout << " + ";
			}
		}
		if (!generic.imposed_constraints.empty()) {
			std::cout << " (imposed: ";
			size_t count = 0;
			for (auto const& constraint : generic.imposed_constraints) {
				if (std::holds_alternative<TypeInfo::Generic::TraitConstraint>(constraint)) {
					auto const& trait_constraint
						= std::get<TypeInfo::Generic::TraitConstraint>(constraint);
					std::cout << symbol_pool_.at(trait_constraint.name).name;
					if (!trait_constraint.arguments.empty()) {
						std::cout << '<';
						size_t subcount = 0;
						for (TypeInfo::ID subtype : trait_constraint.arguments) {
							debug_print_type(subtype);
							if (++subcount < trait_constraint.arguments.size())
								std::cout << ", ";
						}
						std::cout << '>';
					}
				} else debug_print_type(std::get<TypeInfo::Generic::TypeConstraint>(constraint).type);
				if (++count < generic.declared_constraints.size()) std::cout << " + ";
			}
			std::cout << ')';
		}
	} else if (type.is_member_access()) {
		TypeInfo::MemberAccess const& member_access = type.get_member_access();
		debug_print_type(member_access.accessee);
		std::cout << '.' << member_access.field;
		if (!member_access.possible_types.empty()) {
			std::cout << " (possibly: ";
			size_t count = 0;
			for (TypeInfo::ID subid : member_access.possible_types) {
				debug_print_type(subid);
				if (++count < member_access.possible_types.size()) std::cout << ", ";
			}
			std::cout << ')';
		}
	} else if (type.is_named()) {
		TypeInfo::Named const& named = type.get_named();

		if (std::holds_alternative<std::vector<AST::SymbolID>>(named.name)) {
			auto const& ids = std::get<std::vector<AST::SymbolID>>(named.name);
			if (ids.size() == 1) {
				std::cout << symbol_pool_.at(ids[0]).name;
			} else {
				std::cout << '(';
				size_t count = 0;
				for (AST::SymbolID id : ids) {
					std::cout << symbol_pool_.at(id).name;
					if (++count < ids.size()) std::cout << " | ";
				}
				std::cout << ')';
			}
		} else std::cout << *std::get<AST::Identifier const*>(named.name);

		if (!named.generics.empty()) {
			std::cout << '<';
			size_t count = 0;
			for (auto const& [label, id] : named.generics) {
				if (label.has_value()) std::cout << label.value() << ": ";
				debug_print_type(id);
				if (++count < named.generics.size()) std::cout << ", ";
			}
			std::cout << '>';
		}
	} else if (type.is_pointer()) {
		TypeInfo::Pointer const& pointer = type.get_pointer();
		std::cout << "(*" << (pointer.mutable_ ? "mut" : "const") << " ";
		debug_print_type(pointer.pointee);
		std::cout << ")";
	} else if (type.is_known_integer()) {
		AST::Type::Atom::Integer integer = type.get_known_integer().integer;
		std::cout << AST::Type::Atom::make_integer(std::move(integer));
	} else if (type.is_known_float()) {
		std::cout << AST::Type::Atom::make_float(type.get_known_float().width);
	} else if (type.is_partial_integer()) {
		AST::Type::Atom::Integer integer = type.get_partial_integer().integer;

		bool signed_is_known = type.get_partial_integer().signed_is_known;

		std::cout << (signed_is_known ? (integer.is_signed() ? "" : "u") : "?") << "int";
		switch (integer.width_type()) {
		case AST::Type::Atom::Integer::WidthType::Fixed: std::cout << integer.bit_width().value(); break;
		case AST::Type::Atom::Integer::WidthType::Any:   break;
		case AST::Type::Atom::Integer::WidthType::Ptr:   std::cout << "ptr"; break;
		case AST::Type::Atom::Integer::WidthType::Size:  std::cout << "size"; break;
		}
	}
}

std::string Resolver::get_type_name(TypeInfo const& type) const {
	switch (type.kind()) {
	case TypeInfo::Kind::Unknown:        return "unknown";
	case TypeInfo::Kind::Bottom:         return "bottom";
	case TypeInfo::Kind::Module:         return "module";
	case TypeInfo::Kind::KnownVoid:      return "void";
	case TypeInfo::Kind::KnownChar:      return "char";
	case TypeInfo::Kind::KnownBool:      return "bool";
	case TypeInfo::Kind::PartialFloat:   return "float";
	case TypeInfo::Kind::Function:
	case TypeInfo::Kind::SameAs:
	case TypeInfo::Kind::Generic:
	case TypeInfo::Kind::MemberAccess:
	case TypeInfo::Kind::Named:
	case TypeInfo::Kind::Pointer:
	case TypeInfo::Kind::KnownInteger:
	case TypeInfo::Kind::KnownFloat:
	case TypeInfo::Kind::PartialInteger: break;
	}

	std::stringstream output {};
	if (type.is_function()) {
		TypeInfo::Function const& function = type.get_function();
		output << "func(";
		size_t count = 0;
		for (auto const& [name, arg_type] : function.arguments) {
			output << (name.has_value() ? name.value() : "_") << ": ";
			output << get_type_name(arg_type);
			if (++count < function.arguments.size()) output << ", ";
		}
		output << ") " << get_type_name(function.return_) << "";
	} else if (type.is_same_as()) {
		if (type.get_same_as().ids.size() > 1) output << '(';
		size_t count = 0;
		for (TypeInfo::ID subid : type.get_same_as().ids) {
			output << get_type_name(subid);
			if (++count < type.get_same_as().ids.size()) output << " | ";
		}
		if (type.get_same_as().ids.size() > 1) output << ')';
	} else if (type.is_generic()) {
		TypeInfo::Generic const& generic = type.get_generic();
		output << symbol_pool_.at(generic.name).name;
		if (!generic.declared_constraints.empty()) {
			output << ": ";
			size_t count = 0;
			for (auto const& constraint : generic.declared_constraints) {
				output << symbol_pool_.at(constraint.name).name;
				if (!constraint.arguments.empty()) {
					output << '<';
					size_t subcount = 0;
					for (TypeInfo::ID subtype : constraint.arguments) {
						output << get_type_name(subtype);
						if (++subcount < constraint.arguments.size()) output << ", ";
					}
					output << '>';
				}
				if (++count < generic.declared_constraints.size()) output << " + ";
			}
		}
	} else if (type.is_member_access()) {
		TypeInfo::MemberAccess const& member_access = type.get_member_access();
		output << get_type_name(member_access.accessee) << '.' << member_access.field;
	} else if (type.is_named()) {
		TypeInfo::Named const& named = type.get_named();
		// we shouldn't be calling this function pre-partial pruning
		assert(std::holds_alternative<std::vector<AST::SymbolID>>(named.name));

		auto const& ids = std::get<std::vector<AST::SymbolID>>(named.name);
		if (ids.size() == 1) {
			output << symbol_pool_.at(ids[0]).name;
		} else {
			output << '(';
			size_t count = 0;
			for (AST::SymbolID id : ids) {
				output << symbol_pool_.at(id).name;
				if (++count < ids.size()) output << " | ";
			}
			output << ')';
		}

		if (!named.generics.empty()) {
			output << '<';
			size_t count = 0;
			for (auto const& [label, id] : named.generics) {
				if (label.has_value()) output << label.value() << ": ";
				output << get_type_name(id);
				if (++count < named.generics.size()) output << ", ";
			}
			output << '>';
		}
	} else if (type.is_pointer()) {
		TypeInfo::Pointer const& pointer = type.get_pointer();
		output << "*" << (pointer.mutable_ ? "mut" : "const") << " " << get_type_name(pointer.pointee);
	} else if (type.is_known_integer()) {
		AST::Type::Atom::Integer integer = type.get_known_integer().integer;
		output << AST::Type::Atom::make_integer(std::move(integer));
	} else if (type.is_known_float()) {
		output << AST::Type::Atom::make_float(type.get_known_float().width);
	} else if (type.is_partial_integer()) {
		AST::Type::Atom::Integer integer = type.get_partial_integer().integer;

		bool signed_is_known = type.get_partial_integer().signed_is_known;

		output << (signed_is_known ? (integer.is_signed() ? "" : "u") : "(u)") << "int";
		switch (integer.width_type()) {
		case AST::Type::Atom::Integer::WidthType::Fixed: output << integer.bit_width().value(); break;
		case AST::Type::Atom::Integer::WidthType::Any:   break;
		case AST::Type::Atom::Integer::WidthType::Ptr:   output << "ptr"; break;
		case AST::Type::Atom::Integer::WidthType::Size:  output << "size"; break;
		}
	}

	return output.str();
}

std::string Resolver::get_type_name(TypeInfo::ID id) const {
	return get_type_name(type_pool_.at(id));
}

Diagnostic::Sample Resolver::get_type_sample(TypeInfo::ID id, OutFmt::Color color) const {
	return Diagnostic::Sample(
		get_context(get_type_file_id(id)),
		{Diagnostic::Sample::Label(get_type_span(id), get_type_name(id), color)}
	);
}

Resolver::TypeInfo::ID Resolver::type_next() {
	return type_counter_++;
}

Resolver::TypeInfo::ID
Resolver::register_type(TypeInfo&& type, Span span, FileContext::ID file_id, std::optional<AST::SymbolID> symbol_id) {
	TypeInfo::ID id = type_next();
	assert(type_pool_.size() == id);
	assert(type_span_pool_.size() == id);
	assert(type_symbol_mapping_.size() == id);
	type_pool_.push_back(std::move(type));
	type_span_pool_.push_back({span, file_id});
	type_symbol_mapping_.push_back(symbol_id);
	return id;
}

std::vector<AST::SymbolID> Resolver::get_operator_candidates(Token::Symbol operator_, bool binary) const {
	std::vector<AST::SymbolID> symbols {};
	for (Symbol const& symbol : symbol_pool_) {
		// we store operators with the name of the symbol for now (this will most likely change)
		if (symbol.name != get_variant_name(operator_)) continue;
		// this should never fail
		if (!type_pool_.at(symbol.type).is_function()) continue;
		if (type_pool_.at(symbol.type).get_function().arguments.size() != (binary ? 2 : 1)) continue;
		symbols.push_back(symbol.id);
	}
	return symbols;
}

void Resolver::set_same_as(TypeInfo::ID to, TypeInfo::ID from) {
	// set_same_as(a, a) is a noop
	if (to == from) return;
	// the normal case works as always
	if (!type_pool_.at(from).is_same_as()) {
		type_pool_.at(to) = TypeInfo::make_same_as(from);
		return;
	}
	// for a single candidate, we simply go through and check what it is
	if (type_pool_.at(from).get_same_as().ids.size() == 1) {
		set_same_as(to, type_pool_.at(from).get_same_as().ids.at(0));
		return;
	}
	// for many candidates, we don't do anything yet
	// TODO: filter ourselves out from this pool
	type_pool_.at(to) = type_pool_.at(from);
}

Resolver::TypeInfo Resolver::unify_follow_references(
	TypeInfo::ID    same_as,
	TypeInfo::ID    other,
	TypeInfo::ID    same_as_origin,
	TypeInfo::ID    other_origin,
	FileContext::ID file_id
) {
	assert(type_pool_.at(same_as).is_same_as());
	std::vector<TypeInfo::ID> const& ids = type_pool_.at(same_as).get_same_as().ids;

	// if we have a single id, unify it as normal
	if (ids.size() == 1) {
		unify(ids[0], other, same_as_origin, other_origin, file_id);
		return TypeInfo::make_same_as(ids[0]);
	}

	// if we have more than one, filter the non-unifiable ones out
	std::vector<TypeInfo::ID> new_ids {};
	std::copy_if(ids.cbegin(), ids.cend(), std::back_inserter(new_ids), [this, other](TypeInfo::ID id) {
		return can_unify(id, other);
	});

	// if none can be unified, throw a diagnostic and return Bottom
	if (new_ids.empty()) {
		std::stringstream subtitle_stream {};
		subtitle_stream
			<< "none of the candidates for the type "
			<< get_type_name(same_as)
			<< " are compatible with the type "
			<< get_type_name(other);
		parsed_files.at(file_id).diagnostics.push_back(
			Diagnostic::error(
				"type mismatch",
				subtitle_stream.str(),
				{get_type_sample(same_as_origin, OutFmt::Color::Cyan),
		                 get_type_sample(other_origin, OutFmt::Color::Yellow)}
			)
		);
		return TypeInfo::make_bottom();
	}

	// unify all remaining ids and create a new SameAs
	for (TypeInfo::ID new_id : new_ids) unify(new_id, other, same_as_origin, other_origin, file_id);
	return TypeInfo::make_same_as(std::move(new_ids));
}

bool Resolver::unify_basic_known(
	TypeInfo::Kind  kind,
	TypeInfo::ID    a_id,
	TypeInfo::ID    b_id,
	TypeInfo::ID    a_origin,
	TypeInfo::ID    b_origin,
	FileContext::ID file_id
) {
	TypeInfo &a = type_pool_.at(a_id), &b = type_pool_.at(b_id);

	bool a_matches = a.kind() == kind, b_matches = b.kind() == kind;
	if (!a_matches && !b_matches) return false;  // if neither match, this case is not for us
	if (a_matches && b_matches) return true;     // if both match, this is a freebie
	// we add a diagnostic
	std::stringstream subtitle_stream {};
	subtitle_stream
		<< "expected both types to be "
		<< (a_matches ? get_type_name(a_id) : get_type_name(b_id))
		<< "; got "
		<< (a_matches ? get_type_name(b_id) : get_type_name(a_id));
	parsed_files.at(file_id).diagnostics.push_back(
		Diagnostic::error(
			"type mismatch",
			subtitle_stream.str(),
			{get_type_sample(a_matches ? a_origin : b_origin, OutFmt::Color::Cyan),
	                 get_type_sample(a_matches ? b_origin : a_origin, OutFmt::Color::Yellow)}
		)
	);
	// whichever didn't match becomes a bottom
	if (!a_matches) a = TypeInfo::make_bottom();
	if (!b_matches) b = TypeInfo::make_bottom();
	return true;
}

void Resolver::unify_generics(
	TypeInfo::ID    generic,
	TypeInfo::ID    other,
	TypeInfo::ID    generic_origin,
	TypeInfo::ID    other_origin,
	FileContext::ID file_id
) {
	// this is an infallible operation
	assert(type_pool_.at(generic).is_generic());
	auto& a = type_pool_.at(generic).get_generic();

	// if the other type isn't a generic, this adds new imposed constraints to the generic
	if (!type_pool_.at(other).is_generic()) {
		a.imposed_constraints.push_back(TypeInfo::Generic::TypeConstraint {other_origin});
		return;
	}

	// if it is also a generic, we push each generic's declared constraints as imposed constraints for the other
	// generic
	auto& b = type_pool_.at(other).get_generic();

	// TODO: dedup
	for (auto const& constraint : a.declared_constraints) { b.imposed_constraints.push_back(constraint); }

	for (auto const& constraint : b.declared_constraints) { a.imposed_constraints.push_back(constraint); }
}

void Resolver::unify_member_access(
	TypeInfo::ID    member_access,
	TypeInfo::ID    other,
	TypeInfo::ID    member_access_origin,
	TypeInfo::ID    other_origin,
	FileContext::ID file_id
) {
	// for member access, the diagnostics will be thrown later anyways, so we always accept whatever happens here.
	assert(type_pool_.at(member_access).is_member_access());
	auto& a = type_pool_.at(member_access).get_member_access();

	// we push origins for diagnostics' sake
	if (!type_pool_.at(other).is_member_access()) {
		if (std::find(a.possible_types.cbegin(), a.possible_types.cend(), other_origin)
		    == a.possible_types.cend()) {
			a.possible_types.push_back(other_origin);
		}
		return;
	}

	auto& b = type_pool_.at(other).get_member_access();

	// make a list of all common possible types
	std::vector<TypeInfo::ID> new_possible_types {};
	new_possible_types.reserve(a.possible_types.size());
	std::copy(a.possible_types.cbegin(), a.possible_types.cend(), std::back_inserter(new_possible_types));
	std::copy_if(
		b.possible_types.cbegin(),
		b.possible_types.cend(),
		std::back_inserter(new_possible_types),
		[&new_possible_types](TypeInfo::ID type) {
			return std::find(new_possible_types.cbegin(), new_possible_types.cend(), type)
		            == new_possible_types.cend();
		}
	);

	// then set it for both!
	a.possible_types = new_possible_types;
	b.possible_types = std::move(new_possible_types);
}

void Resolver::unify_functions(
	TypeInfo::ID    function,
	TypeInfo::ID    other,
	TypeInfo::ID    function_origin,
	TypeInfo::ID    other_origin,
	FileContext::ID file_id
) {
	// ensure they're both functions
	assert(type_pool_.at(function).is_function());
	if (!type_pool_.at(other).is_function()) {
		std::stringstream subtitle_stream {};
		subtitle_stream << "expected both types to be functions; got " << get_type_name(other);
		parsed_files.at(file_id).diagnostics.push_back(
			Diagnostic::error(
				"type mismatch",
				subtitle_stream.str(),
				{get_type_sample(function_origin, OutFmt::Color::Cyan),
		                 get_type_sample(other_origin, OutFmt::Color::Yellow)}
			)
		);
		return;
	}

	TypeInfo::Function &a_function = type_pool_.at(function).get_function(),
			   &b_function = type_pool_.at(other).get_function();

	// ensure they have the same argument count
	if (a_function.arguments.size() != b_function.arguments.size()) {
		parsed_files.at(file_id).diagnostics.push_back(
			Diagnostic::error(
				"type mismatch",
				"expected both functions to have the same amount of arguments",
				{get_type_sample(function_origin, OutFmt::Color::Cyan),
		                 get_type_sample(other_origin, OutFmt::Color::Yellow)}
			)
		);
		return;
	}

	// unify the names and types of arguments
	for (size_t i = 0; i < a_function.arguments.size(); ++i) {
		auto& [a_name, a_type] = a_function.arguments.at(i);
		auto& [b_name, b_type] = b_function.arguments.at(i);

		// unify the type first for diagnostics' sake
		bool can_unify_type = can_unify(a_type, b_type);
		unify(a_type, b_type, file_id);

		bool a_has_name = a_name.has_value(), b_has_name = b_name.has_value();
		if (a_has_name && b_has_name && (a_name.value() != b_name.value())) {
			// we don't want to throw the diagnostic if the type cannot be unified to begin with
			if (can_unify_type) {
				// FIXME: we don't have the span/fileid for the argument, so we can't show a sample!
				parsed_files.at(file_id).diagnostics.push_back(
					Diagnostic::error(
						"type mismatch",
						"expected both functions to have the same argument names, since neither argument name is marked as anonymous",
						{}
					)
				);
			}
		} else if (a_has_name || b_has_name) {
			if (a_has_name) b_name = a_name;
			if (b_has_name) a_name = b_name;
		}
	}

	// unify the return types
	unify(a_function.return_, b_function.return_, file_id);
	return;
}

void Resolver::unify_pointers(
	TypeInfo::ID    pointer,
	TypeInfo::ID    other,
	TypeInfo::ID    pointer_origin,
	TypeInfo::ID    other_origin,
	FileContext::ID file_id
) {
	assert(type_pool_.at(pointer).is_pointer());
	if (!type_pool_.at(other).is_pointer()) {
		std::stringstream subtitle_stream {};
		subtitle_stream << "expected both types to be pointers; got " << get_type_name(other);
		parsed_files.at(file_id).diagnostics.push_back(
			Diagnostic::error(
				"type mismatch",
				subtitle_stream.str(),
				{get_type_sample(pointer_origin, OutFmt::Color::Cyan),
		                 get_type_sample(other_origin, OutFmt::Color::Yellow)}
			)
		);
		return;
	}

	TypeInfo::Pointer const &a_pointer = type_pool_.at(pointer).get_pointer(),
				&b_pointer = type_pool_.at(other).get_pointer();

	// ensure they point to the same type
	unify(a_pointer.pointee, b_pointer.pointee, pointer_origin, other_origin, file_id);

	// ensure they are of the same mutability
	if (a_pointer.mutable_ != b_pointer.mutable_) {
		parsed_files.at(file_id).diagnostics.push_back(
			Diagnostic::error(
				"type mismatch",
				"expected both pointers to have the same mutability",
				{get_type_sample(pointer_origin, OutFmt::Color::Cyan),
		                 get_type_sample(other_origin, OutFmt::Color::Yellow)}
			)
		);
		return;
	}
}

void Resolver::unify_named(
	TypeInfo::ID    named,
	TypeInfo::ID    other,
	TypeInfo::ID    named_origin,
	TypeInfo::ID    other_origin,
	FileContext::ID file_id
) {
	assert(type_pool_.at(named).is_named());
	if (!type_pool_.at(other).is_named()) {
		std::stringstream subtitle_stream {};
		subtitle_stream
			<< "expected both types to be "
			<< get_type_name(named)
			<< "; got "
			<< get_type_name(other);
		parsed_files.at(file_id).diagnostics.push_back(
			Diagnostic::error(
				"type mismatch",
				subtitle_stream.str(),
				{get_type_sample(named_origin, OutFmt::Color::Cyan),
		                 get_type_sample(other_origin, OutFmt::Color::Yellow)}
			)
		);
		return;
	}

	// TODO: unify
	/*
	AST::SymbolID a = type_pool_.at(named).get_named_known(), b = type_pool_.at(other).get_named_known();

	// ensure they are actually the same named type
	if (a != b) {
		std::stringstream subtitle_stream {};
		subtitle_stream
			<< "expected both types to be "
			<< get_type_name(named)
			<< "; got "
			<< get_type_name(other);
		parsed_files.at(file_id).diagnostics.push_back(
			Diagnostic::error(
				"type mismatch",
				subtitle_stream.str(),
				{get_type_sample(named_origin, OutFmt::Color::Cyan),
		                 get_type_sample(other_origin, OutFmt::Color::Yellow)}
			)
		);
		return;
	}
	*/
}

void Resolver::unify(
	TypeInfo::ID    a_id,
	TypeInfo::ID    b_id,
	TypeInfo::ID    a_origin,
	TypeInfo::ID    b_origin,
	FileContext::ID file_id
) {
	TypeInfo &a = type_pool_.at(a_id), &b = type_pool_.at(b_id);

	// follow references
	if (a.is_same_as()) {
		a = unify_follow_references(a_id, b_id, a_origin, b_origin, file_id);
		return;
	}
	if (b.is_same_as()) {
		b = unify_follow_references(b_id, a_id, b_origin, a_origin, file_id);
		return;
	}

	// bottoms don't participate in unification
	if (a.is_bottom() || b.is_bottom()) return;

	// make unknowns known
	if (a.is_unknown()) {
		set_same_as(a_id, b_id);
		return;
	}
	if (b.is_unknown()) {
		set_same_as(b_id, a_id);
		return;
	}

	// generics
	if (a.is_generic()) return unify_generics(a_id, b_id, a_origin, b_origin, file_id);
	if (b.is_generic()) return unify_generics(b_id, a_id, b_origin, a_origin, file_id);

	// member access types
	if (a.is_member_access()) return unify_member_access(a_id, b_id, a_origin, b_origin, file_id);
	if (b.is_member_access()) return unify_member_access(b_id, a_id, b_origin, a_origin, file_id);

	// named types
	if (a.is_named()) return unify_named(a_id, b_id, a_origin, b_origin, file_id);
	if (b.is_named()) return unify_named(b_id, a_id, b_origin, a_origin, file_id);

	// if any of them is a basic Known type, the other must be exactly the same
	if (unify_basic_known(TypeInfo::Kind::KnownVoid, a_id, b_id, a_origin, b_origin, file_id)) return;
	if (unify_basic_known(TypeInfo::Kind::KnownChar, a_id, b_id, a_origin, b_origin, file_id)) return;
	if (unify_basic_known(TypeInfo::Kind::KnownBool, a_id, b_id, a_origin, b_origin, file_id)) return;

	// modules act like basic Known types right now, but this is silly.
	// TODO: do something better
	if (unify_basic_known(TypeInfo::Kind::Module, a_id, b_id, a_origin, b_origin, file_id)) return;

	// functions
	if (a.is_function()) return unify_functions(a_id, b_id, a_origin, b_origin, file_id);
	if (b.is_function()) return unify_functions(b_id, a_id, b_origin, a_origin, file_id);

	// pointers
	if (a.is_pointer()) return unify_pointers(a_id, b_id, a_origin, b_origin, file_id);
	if (b.is_pointer()) return unify_pointers(b_id, a_id, b_origin, a_origin, file_id);

	// now only numeric types are left ([Known/Partial][Integer/Float])
	bool a_known = a.is_known_integer() || a.is_known_float(), b_known = b.is_known_integer() || b.is_known_float();
	bool a_float = a.is_known_float() || a.is_partial_float(), b_float = b.is_known_float() || b.is_partial_float();

	// both must be either floats or integers
	if (a_float != b_float) {
		parsed_files.at(file_id).diagnostics.push_back(
			Diagnostic::error(
				"type mismatch",
				"incompatible numeric types: either the float must be truncated to an integer or the integer must be cast to a float",
				{get_type_sample(a_origin, OutFmt::Color::Cyan),
		                 get_type_sample(b_origin, OutFmt::Color::Yellow)}
			)
		);
		return;
	}

	// floats
	if (a_float) {
		// if neither are known, there is no information to be learnt, since we can only learn the bit
		// width
		if (!a_known && !b_known) return;

		// if both are known, we must ensure they don't clash
		if (a_known && b_known) {
			if (a.get_known_float().width != b.get_known_float().width) {
				parsed_files.at(file_id).diagnostics.push_back(
					Diagnostic::error(
						"type mismatch",
						"incompatible numeric types: floats of incompatible size",
						{get_type_sample(a_origin, OutFmt::Color::Cyan),
				                 get_type_sample(b_origin, OutFmt::Color::Yellow)}
					)
				);
				return;
			}
		}

		// if one is known, we can make the other the same as the other
		if (a_known) set_same_as(b_id, a_id);
		if (b_known) set_same_as(a_id, b_id);
		return;
	}

	// integers may clash due to sign and size
	bool signed_clash = false, size_clash = false;
	if (a_known && b_known) {
		// if both are known, we can just check clashes directly
		if (a.get_known_integer().integer.is_signed() != b.get_known_integer().integer.is_signed())
			signed_clash = true;
		if (a.get_known_integer().integer.width_type() != b.get_known_integer().integer.width_type())
			size_clash = true;
		if (a.get_known_integer().integer.bit_width() != b.get_known_integer().integer.bit_width())
			size_clash = true;
	} else if (a_known != b_known) {
		// if one isn't known, we must check that there are no clashes
		TypeInfo::PartialInteger const& partial = a_known ? b.get_partial_integer() : a.get_partial_integer();
		TypeInfo::KnownInteger const&   known   = a_known ? a.get_known_integer() : b.get_known_integer();

		// if the sign is known, it must not clash
		if (partial.signed_is_known && partial.integer.is_signed() != known.integer.is_signed())
			signed_clash = true;

		// if the size is known, it must not clash
		if (partial.integer.width_type() != AST::Type::Atom::Integer::WidthType::Any) {
			// the width types must match
			if (partial.integer.width_type() != known.integer.width_type()) size_clash = true;
			// if both are fixed width, their widths must match
			else if (partial.integer.bit_width() != known.integer.bit_width()) size_clash = true;
		}

		// if no clashes were detected, we unify by setting the partial one to the known one
		if (!signed_clash && !size_clash) { set_same_as(a_known ? b_id : a_id, a_known ? a_id : b_id); }
	} else {
		// neither are known, so we will construct a unified partial integer for both of them
		TypeInfo::PartialInteger const &a_partial = a.get_partial_integer(),
					       &b_partial = b.get_partial_integer();

		// let's determine the sign
		bool signed_ = false, signed_is_known;
		if (a_partial.signed_is_known && b_partial.signed_is_known) {
			// if the sign is known for both, it must not clash
			if (a_partial.integer.is_signed() != b_partial.integer.is_signed()) signed_clash = true;
			else {
				signed_is_known = true;
				signed_         = a_partial.integer.is_signed();
			}
		} else if (a_partial.signed_is_known != b_partial.signed_is_known) {
			// if the sign is known for one of them, we infer it from there
			signed_is_known = true;
			signed_         = a_partial.signed_is_known ? a_partial.integer.is_signed()
			                                            : b_partial.integer.is_signed();
		} else {
			// if it isn't known for either, we don't know anything
			signed_is_known = false;
		}

		// let's determine the width
		uint32_t width      = 0;
		auto     width_type = AST::Type::Atom::Integer::WidthType::Any;

		bool a_know_width = a_partial.integer.width_type() != AST::Type::Atom::Integer::WidthType::Any,
		     b_know_width = b_partial.integer.width_type() != AST::Type::Atom::Integer::WidthType::Any;
		if (a_know_width && b_know_width) {
			// if the width is known for both, it must not clash
			if (a_partial.integer.width_type() != b_partial.integer.width_type()) size_clash = true;
			else if (a_partial.integer.bit_width() != b_partial.integer.bit_width()) size_clash = true;
			else {
				width_type = a_partial.integer.width_type();
				if (width_type == AST::Type::Atom::Integer::WidthType::Fixed)
					width = a_partial.integer.bit_width().value();
			}
		} else if (a_know_width != b_know_width) {
			// if the width is known for only one of them, we infer it from there
			width_type = a_know_width ? a_partial.integer.width_type() : b_partial.integer.width_type();
			if (width_type == AST::Type::Atom::Integer::WidthType::Fixed)
				width = a_know_width ? a_partial.integer.bit_width().value()
				                     : b_partial.integer.bit_width().value();
		}

		if (!signed_clash && !size_clash) {
			// create the unified integer type
			AST::Type::Atom::Integer integer
				= width_type == AST::Type::Atom::Integer::WidthType::Any
			                ? AST::Type::Atom::Integer::any(signed_)
			        : width_type == AST::Type::Atom::Integer::WidthType::Size
			                ? AST::Type::Atom::Integer::size(signed_)
			        : width_type == AST::Type::Atom::Integer::WidthType::Ptr
			                ? AST::Type::Atom::Integer::ptr(signed_)
			                : AST::Type::Atom::Integer::with_width(width, signed_).value();

			TypeInfo new_type;
			if (signed_is_known && width_type != AST::Type::Atom::Integer::WidthType::Any) {
				// we know everything
				new_type = TypeInfo::make_known_integer(TypeInfo::KnownInteger {integer});
			} else {
				// we don't know everything, so this must be partial
				new_type = TypeInfo::make_partial_integer(
					TypeInfo::PartialInteger {integer, signed_is_known}
				);
			}

			// this is arbitrary, it works both ways
			b = new_type;
			a = TypeInfo::make_same_as(b_id);
		}
	}

	if (signed_clash)
		parsed_files.at(file_id).diagnostics.push_back(
			Diagnostic::error(
				"type mismatch",
				"incompatible numeric types: integers of incompatible sign",
				{get_type_sample(a_origin, OutFmt::Color::Cyan),
		                 get_type_sample(b_origin, OutFmt::Color::Yellow)}
			)
		);
	if (size_clash)
		parsed_files.at(file_id).diagnostics.push_back(
			Diagnostic::error(
				"type mismatch",
				"incompatible numeric types: integers of incompatible size",
				{get_type_sample(a_origin, OutFmt::Color::Cyan),
		                 get_type_sample(b_origin, OutFmt::Color::Yellow)}
			)
		);
}

void Resolver::unify(TypeInfo::ID a_id, TypeInfo::ID b_id, FileContext::ID file_id) {
	return unify(a_id, b_id, a_id, b_id, file_id);
}

bool Resolver::can_unify_follow_references(TypeInfo const& same_as, TypeInfo const& other) const {
	assert(same_as.is_same_as());
	std::vector<TypeInfo::ID> const& ids = same_as.get_same_as().ids;
	return std::any_of(ids.cbegin(), ids.cend(), [this, other](TypeInfo::ID id) { return can_unify(id, other); });
}

std::optional<bool> Resolver::can_unify_basic_known(TypeInfo::Kind kind, TypeInfo const& a, TypeInfo const& b) const {
	bool a_matches = a.kind() == kind, b_matches = b.kind() == kind;
	if (!a_matches && !b_matches) return std::nullopt;  // if neither match, this case is not for us
	if (a_matches && b_matches) return true;            // if both match, this is a freebie
	return false;                                       // if neither match, it won't work
}

bool Resolver::can_unify_functions(TypeInfo const& function, TypeInfo const& other) const {
	// ensure they're both functions
	assert(function.is_function());
	if (!other.is_function()) { return false; }

	TypeInfo::Function const &a_function = function.get_function(), &b_function = other.get_function();

	// ensure they have the same argument count
	if (a_function.arguments.size() != b_function.arguments.size()) { return false; }

	// unify the names and types of arguments
	for (size_t i = 0; i < a_function.arguments.size(); ++i) {
		auto& [a_name, a_type] = a_function.arguments.at(i);
		auto& [b_name, b_type] = b_function.arguments.at(i);
		if (a_name.has_value() && b_name.has_value() && (a_name.value() != b_name.value())) { return false; }
		if (!can_unify(a_type, b_type)) return false;
	}

	// unify the return types
	return can_unify(a_function.return_, b_function.return_);
}

bool Resolver::can_unify_pointers(TypeInfo const& pointer, TypeInfo const& other) const {
	// ensure they're both pointers
	assert(pointer.is_pointer());
	if (!other.is_pointer()) { return false; }

	TypeInfo::Pointer const &a_pointer = pointer.get_pointer(), &b_pointer = other.get_pointer();

	// ensure they point to the same type
	if (!can_unify(a_pointer.pointee, b_pointer.pointee)) return false;

	// ensure they are of the same mutability
	return a_pointer.mutable_ == b_pointer.mutable_;
}

bool Resolver::can_unify_named(TypeInfo const& named, TypeInfo const& other) const {
	// ensure they're both named types
	assert(named.is_named());
	if (!other.is_named()) { return false; }

	// TODO: can unify
	/*
	// ensure they are actually the same named type
	return named.get_named_known() == other.get_named_known();
	*/
}

bool Resolver::can_unify(TypeInfo::ID a, TypeInfo::ID b) const {
	return can_unify(type_pool_.at(a), type_pool_.at(b));
}

bool Resolver::can_unify(TypeInfo const& a, TypeInfo::ID b) const {
	return can_unify(a, type_pool_.at(b));
}

bool Resolver::can_unify(TypeInfo::ID a, TypeInfo const& b) const {
	return can_unify(type_pool_.at(a), b);
}

bool Resolver::can_unify(TypeInfo const& a, TypeInfo const& b) const {
	// follow references
	if (a.is_same_as()) return can_unify_follow_references(a, b);
	if (b.is_same_as()) return can_unify_follow_references(b, a);

	// bottoms don't participate in unification
	if (a.is_bottom() || b.is_bottom()) return true;

	// make unknowns known
	if (a.is_unknown()) return true;
	if (b.is_unknown()) return true;

	// generics always unify
	if (a.is_generic()) return true;
	if (b.is_generic()) return true;

	// member access types (these always succeed :P)
	if (a.is_member_access()) return true;
	if (b.is_member_access()) return true;

	// named types
	if (a.is_named()) return can_unify_named(a, b);
	if (b.is_named()) return can_unify_named(b, a);

	// if any of them is a basic Known type, the other must be exactly the same
	std::optional<bool> attempt;
	attempt = can_unify_basic_known(TypeInfo::Kind::KnownVoid, a, b);
	if (attempt.has_value()) return attempt.value();
	attempt = can_unify_basic_known(TypeInfo::Kind::KnownChar, a, b);
	if (attempt.has_value()) return attempt.value();
	attempt = can_unify_basic_known(TypeInfo::Kind::KnownBool, a, b);
	if (attempt.has_value()) return attempt.value();

	// modules act like basic Known types right now, but this is silly.
	// TODO: do something better
	attempt = can_unify_basic_known(TypeInfo::Kind::Module, a, b);
	if (attempt.has_value()) return attempt.value();

	// functions
	if (a.is_function()) return can_unify_functions(a, b);
	if (b.is_function()) return can_unify_functions(b, a);

	// pointers
	if (a.is_pointer()) return can_unify_pointers(a, b);
	if (b.is_pointer()) return can_unify_pointers(b, a);

	// now only numeric types are left ([Known/Partial][Integer/Float])
	bool a_known = a.is_known_integer() || a.is_known_float(), b_known = b.is_known_integer() || b.is_known_float();
	bool a_float = a.is_known_float() || a.is_partial_float(), b_float = b.is_known_float() || b.is_partial_float();

	// both must be either floats or integers
	if (a_float != b_float) return false;

	// floats
	if (a_float) {
		if (!a_known && !b_known) return true;
		if (a_known && b_known && a.get_known_float().width != b.get_known_float().width) return false;
		return true;
	}

	// integers may clash due to sign and size
	if (a_known && b_known) {
		// if both are known, we can just check clashes directly
		if (a.get_known_integer().integer.is_signed() != b.get_known_integer().integer.is_signed())
			return false;
		if (a.get_known_integer().integer.width_type() != b.get_known_integer().integer.width_type())
			return false;
		if (a.get_known_integer().integer.bit_width() != b.get_known_integer().integer.bit_width())
			return false;
		return true;
	} else if (a_known != b_known) {
		// if one isn't known, we must check that there are no clashes
		TypeInfo::PartialInteger const& partial = a_known ? b.get_partial_integer() : a.get_partial_integer();
		TypeInfo::KnownInteger const&   known   = a_known ? a.get_known_integer() : b.get_known_integer();

		// if the sign is known, it must not clash
		if (partial.signed_is_known && partial.integer.is_signed() != known.integer.is_signed()) return false;

		// if the size is known, it must not clash
		if (partial.integer.width_type() != AST::Type::Atom::Integer::WidthType::Any) {
			if (partial.integer.width_type() != known.integer.width_type()) return false;
			else if (partial.integer.bit_width() != known.integer.bit_width()) return false;
		}

		// if no clashes were detected, we unify by setting the partial one to the known one
		return true;
	} else {
		// neither are known, so we will construct a unified partial integer for both of them
		TypeInfo::PartialInteger const &a_partial = a.get_partial_integer(),
					       &b_partial = b.get_partial_integer();

		// if the sign is known for both, it must not clash
		if (a_partial.signed_is_known
		    && b_partial.signed_is_known
		    && a_partial.integer.is_signed() != b_partial.integer.is_signed())
			return false;

		// if the width is known for both, it must not clash
		if (a_partial.integer.width_type() != AST::Type::Atom::Integer::WidthType::Any
		    && b_partial.integer.width_type() != AST::Type::Atom::Integer::WidthType::Any) {
			if (a_partial.integer.width_type() != b_partial.integer.width_type()) return false;
			else if (a_partial.integer.bit_width() != b_partial.integer.bit_width()) return false;
		}
		return true;
	}
}

bool Resolver::try_decide(UndecidedOverload& undecided_overload) {
	// filter the candidates
	std::vector<UndecidedOverload::Candidate> new_candidates {};
	for (UndecidedOverload::Candidate& candidate : undecided_overload.candidates) {
		if (can_unify(candidate.call_type, candidate.function)) {
			new_candidates.push_back(std::move(candidate));
		} else {
			// TODO: specify how it is incompatible?
			std::stringstream text {};
			text
				<< "function signature ("
				<< get_type_name(candidate.function)
				<< ") is incompatible with the function call signature ("
				<< get_type_name(candidate.call_type)
				<< ")";
			undecided_overload.rejections.push_back(
				Diagnostic::Sample(
					get_context(get_type_file_id(candidate.function)),
					{Diagnostic::Sample::Label(
						get_type_span(candidate.function),
						text.str(),
						OutFmt::Color::Magenta
					)}
				)
			);
		}
	}
	undecided_overload.candidates = std::move(new_candidates);

	// if no candidates are unifiable, it's unresolved.
	if (undecided_overload.candidates.empty()) {
		parsed_files.at(undecided_overload.file_id)
			.diagnostics.push_back(
				Diagnostic::error(
					"could not resolve function overload",
					"no function matched the constraints imposed by the function call",
					std::move(undecided_overload.rejections)
				)
			);

		// we need to "unresolve" the callee identifier just in case
		if (undecided_overload.identifier.has_value()) undecided_overload.identifier.value()->id = {};

		// we also need to set the type to bottom to avoid causing more issues
		type_pool_.at(undecided_overload.expr_type) = TypeInfo::make_bottom();

		return true;
	}

	// if too many candidates are unifiable, we fail to decide.
	if (undecided_overload.candidates.size() > 1) { return false; }

	// if only one is unifiable, we've finally found the one and only function
	assert(undecided_overload.candidates.size() == 1);
	TypeInfo::ID call_id = register_type(
		std::move(undecided_overload.candidates.at(0).call_type),
		undecided_overload.span,
		undecided_overload.file_id
	);
	unify(call_id, undecided_overload.candidates.at(0).function, undecided_overload.file_id);

	// if we're calling an identifier, let's finish resolving it
	if (undecided_overload.identifier.has_value()) {
		if (!type_symbol_mapping_.at(undecided_overload.candidates.at(0).function).has_value()) {
			// TODO: think about when this would ever happen
			std::cout << "error: there is no value for this call ID: ";
			debug_print_type(undecided_overload.candidates.at(0).function);
			std::cout << std::endl;
		} else {
			undecided_overload.identifier.value()->id
				= {type_symbol_mapping_.at(undecided_overload.candidates.at(0).function).value()};
		}
	}

	return true;
}

bool Resolver::try_decide(TypeInfo::ID undecided_member_access) {
	TypeInfo::MemberAccess& member_access = type_pool_.at(undecided_member_access).get_member_access();

	// we can only actually decide the member access if we know the type of the accessee
	std::optional<TypeInfo const*> maybe_underlying
		= type_pool_.at(member_access.accessee).get_single_underlying(type_pool_);
	if (!maybe_underlying.has_value()) return false;

	// only named types can have fields as of right now
	TypeInfo const& underlying = *maybe_underlying.value();
	if (!underlying.is_named()) return false;

	// this member access will 100% be resolved now! :D
	// TODO: no it won't
	std::vector<AST::SymbolID> type_name_ids = std::get<std::vector<AST::SymbolID>>(underlying.get_named().name);
	if (type_name_ids.size() > 1) return false;
	if (type_name_ids.size() < 1) {
		return true;
	}
	AST::SymbolID type_name_id = type_name_ids.at(0);

	// we don't have any other user type as of now :P
	AST::Struct* struct_ = std::get<AST::Struct*>(symbol_pool_.at(type_name_id).item);

	// first of all, let's check whether the struct even has the field
	auto maybe_field = std::find_if(
		struct_->fields.cbegin(),
		struct_->fields.cend(),
		[&member_access](AST::Struct::Field const& field) { return field.name.value == member_access.field; }
	);
	if (maybe_field == struct_->fields.cend()) {
		// if it doesn't exist, we throw a diagnostic
		FileContext::ID   file_id = get_type_file_id(undecided_member_access);
		std::stringstream subtitle_stream {};
		subtitle_stream
			<< "type `"
			<< struct_->name.value.name()
			<< "` does not have any field named `"
			<< member_access.field
			<< "`";
		parsed_files.at(file_id).diagnostics.push_back(
			Diagnostic::error(
				"tried to access nonexistent field",
				subtitle_stream.str(),
				{Diagnostic::Sample(
					get_context(file_id),
					get_type_span(undecided_member_access),
					OutFmt::Color::Red
				)}
			)
		);

		// we need to set the type to bottom to avoid causing more issues
		type_pool_.at(undecided_member_access) = TypeInfo::make_bottom();
		return true;
	}

	// let's get the type of the field
	AST::Struct::Field const& field = *maybe_field;
	TypeInfo field_type             = from_type(field.type.value, symbol_pool_.at(type_name_id).file_id, false);

	// finally, let's set the type and unify all possible types
	type_pool_.at(undecided_member_access) = field_type;
	for (TypeInfo::ID id : member_access.possible_types) {
		unify(id, undecided_member_access, get_type_file_id(id));
	}

	return true;
}

Resolver::TypeInfo::ID
Resolver::infer(AST::Expression::Atom::StructLiteral& struct_literal, Span span, FileContext::ID file_id) {
	// TODO: infer struct literals
	/*
	// the type is whichever struct is pointed to
	AST::SymbolID type_symbol_id = struct_literal.name.value.id.value().at(0);
	TypeInfo      type           = TypeInfo::make_named(type_symbol_id);
	TypeInfo::ID  type_id        = register_type(std::move(type), span, file_id);

	// now, for each field, we unify it with the struct field type
	AST::Struct* struct_ = std::get<AST::Struct*>(symbol_pool_.at(type_symbol_id).item);
	for (auto& field : struct_literal.fields) {
		// we skip those which don't exist
		auto struct_field = std::find_if(
			struct_->fields.cbegin(),
			struct_->fields.cend(),
			[&field](AST::Struct::Field const& given_field) {
				return given_field.name.value == field.name.value;
			}
		);
		if (struct_field == struct_->fields.cend()) continue;

		TypeInfo::ID field_type = infer(field.value->value, field.value->span, file_id);
		// FIXME: we shouldn't have to re-register the types per struct literal
		TypeInfo struct_field_type
			= from_type(struct_field->type.value, symbol_pool_.at(type_symbol_id).file_id, false);
		TypeInfo::ID struct_field_type_id = register_type(
			std::move(struct_field_type),
			struct_field->type.span,
			symbol_pool_.at(type_symbol_id).file_id
		);

		// FIXME: i don't like how these diagnostics are worded for struct fields
		unify(field_type, struct_field_type_id, file_id);
	}

	return type_id;
	*/
}

Resolver::TypeInfo::ID Resolver::infer(AST::Expression::Atom& atom, Span span, FileContext::ID file_id) {
	switch (atom.kind()) {
	case AST::Expression::Atom::Kind::NumberLiteral:
		// TODO: apply suffixes
		if (atom.get_number_literal().is_float())
			return register_type(TypeInfo::make_partial_float(), span, file_id);
		else
			return register_type(
				TypeInfo::make_partial_integer(
					TypeInfo::PartialInteger {AST::Type::Atom::Integer::any(false), false}
				),
				span,
				file_id
			);
	case AST::Expression::Atom::Kind::StringLiteral:
		// TODO: do string literals
		std::cout << "unsupported string literal" << std::endl;
		std::exit(0);
	case AST::Expression::Atom::Kind::CharLiteral:
		// TODO: apply suffixes
		return register_type(TypeInfo::make_known_char(), span, file_id);
	case AST::Expression::Atom::Kind::BoolLiteral:   return register_type(TypeInfo::make_known_bool(), span, file_id);
	case AST::Expression::Atom::Kind::StructLiteral: return infer(atom.get_struct_literal(), span, file_id);
	case AST::Expression::Atom::Kind::Expression:    return infer(*atom.get_expression(), span, file_id);
	case AST::Expression::Atom::Kind::Identifier:    break;
	}

	// for identifiers, we match the type in the symbol pool
	AST::Identifier const& identifier = atom.get_identifier();
	if (!identifier.id.has_value())
		return register_type(
			TypeInfo::make_bottom(),
			span,
			file_id
		);  // if we don't know what this is, let's ignore
	std::vector<AST::SymbolID> const& ids = identifier.id.value();
	if (ids.empty()) return register_type(TypeInfo::make_bottom(), span, file_id);
	std::vector<TypeInfo::ID> type_ids {};
	type_ids.reserve(ids.size());
	std::transform(ids.cbegin(), ids.cend(), std::back_inserter(type_ids), [this](AST::SymbolID id) {
		return get_single_symbol(id).type;
	});
	if (type_ids.size() == 1) return register_type(TypeInfo::make_same_as(type_ids[0]), span, file_id);
	else return register_type(TypeInfo::make_same_as(std::move(type_ids)), span, file_id);
}

Resolver::TypeInfo::ID
Resolver::infer(AST::Expression::FunctionCall& function_call, Span span, FileContext::ID file_id) {
	// for function calls, we need to resolve or partially resolve the overload
	TypeInfo::ID callee_id = infer(function_call.callee->value, function_call.callee->span, file_id);

	// first, we ensure that there is at least one callable item
	if (!type_pool_.at(callee_id).is_callable(type_pool_)) {
		if (!type_pool_.at(callee_id).is_bottom())
			parsed_files.at(file_id).diagnostics.push_back(
				Diagnostic::error(
					"type mismatch",
					"attempted to call a non-function",
					{get_type_sample(callee_id, OutFmt::Color::Red)}
				)
			);
		return register_type(TypeInfo::make_bottom(), span, file_id);
	}

	// now, we flatten this type to a SameAs which points at all the callable types
	std::vector<TypeInfo::ID> callable = type_pool_.at(callee_id).get_callable_subitems(callee_id, type_pool_);
	assert(!callable.empty());  // we know is_callable()

	// we need to further filter this to ensure we have same argument count and our labeled arguments match
	// TODO: this works for now, but will break once arguments have default values
	size_t provided_arguments = function_call.arguments.labeled.size() + function_call.arguments.ordered.size();
	std::vector<TypeInfo::ID> callable_filtered {};
	// this list holds all function rejections as code samples so we can provide a rich diagnostic
	std::vector<Diagnostic::Sample> rejections {};
	// we do add the function call as a sample even though it is not a rejection
	rejections.push_back(
		Diagnostic::Sample(
			get_context(file_id),
			"call site",
			{Diagnostic::Sample::Label(span, "function call", OutFmt::Color::Gray)}
		)
	);
	for (TypeInfo::ID callable_id : callable) {
		assert(type_pool_.at(callable_id).is_function());
		auto const& function_arguments     = type_pool_.at(callable_id).get_function().arguments;
		bool        argument_count_matches = function_arguments.size() == provided_arguments;
		if (!argument_count_matches) {
			rejections.push_back(
				Diagnostic::Sample(
					get_context(get_type_file_id(callable_id)),
					{Diagnostic::Sample::Label(
						get_type_span(callable_id),
						function_arguments.size() < provided_arguments
							? "incompatible argument count (too many were provided)"
							: "incompatible argument count (too few were provided)",
						OutFmt::Color::BrightBlue
					)}
				)
			);
			continue;
		}
		if (!function_call.arguments.labeled.empty()) {
			// check that all labeled arguments exist
			// we only consider arguments that haven't already been provided by the ordered
			// arguments
			std::vector<std::string_view> arguments_under_consideration {};
			arguments_under_consideration.reserve(
				function_arguments.size() - function_call.arguments.ordered.size()
			);
			for (size_t i = function_call.arguments.ordered.size(); i < function_arguments.size(); ++i)
				if (std::get<0>(function_arguments.at(i)).has_value())
					arguments_under_consideration.push_back(
						std::get<0>(function_arguments.at(i)).value()
					);

			bool all_arguments_exist = true;
			for (auto const& argument : function_call.arguments.labeled) {
				auto const& identifier = std::get<0>(argument);

				bool argument_exists = std::any_of(
					arguments_under_consideration.cbegin(),
					arguments_under_consideration.cend(),
					[&identifier](std::string_view actual_argument) {
						return actual_argument == identifier.value.name();
					}
				);

				if (!argument_exists) {
					std::stringstream text {};
					text << "missing labeled argument `" << identifier.value.name() << "`";
					rejections.push_back(
						Diagnostic::Sample(
							get_context(get_type_file_id(callable_id)),
							{Diagnostic::Sample::Label(
								get_type_span(callable_id),
								text.str(),
								OutFmt::Color::BrightGreen
							)}
						)
					);
					all_arguments_exist = false;
					break;
				}
			}
			if (!all_arguments_exist) continue;
		}
		callable_filtered.push_back(callable_id);
	}

	if (callable_filtered.empty()) {
		parsed_files.at(file_id).diagnostics.push_back(
			Diagnostic::error(
				"could not resolve function overload",
				"no function matched the constraints imposed by the function call",
				std::move(rejections)
			)
		);

		// we need to "unresolve" the callee identifier just in case
		if (function_call.callee->value.is_atom() || function_call.callee->value.get_atom().is_identifier())
			function_call.callee->value.get_atom().get_identifier().id = {};

		return register_type(TypeInfo::make_bottom(), span, file_id);
	}

	// it's time to infer the arguments now that we have a set of possible callees
	std::vector<TypeInfo::ID> ordered_arguments {};
	ordered_arguments.reserve(function_call.arguments.ordered.size());
	std::transform(
		function_call.arguments.ordered.begin(),
		function_call.arguments.ordered.end(),
		std::back_inserter(ordered_arguments),
		[this, file_id](auto& argument) { return infer(argument.value, argument.span, file_id); }
	);

	std::unordered_map<std::string, TypeInfo::ID> labeled_arguments {};
	ordered_arguments.reserve(function_call.arguments.labeled.size());
	for (auto& [identifier, value] : function_call.arguments.labeled) {
		labeled_arguments.emplace(identifier.value.name(), infer(value.value, value.span, file_id));
	}

	TypeInfo::ID expr_type = register_type(TypeInfo::make_unknown(), span, file_id);

	// let's store all of the candidates
	std::vector<UndecidedOverload::Candidate> candidates {};
	candidates.reserve(callable_filtered.size());
	for (TypeInfo::ID callable_id : callable_filtered) {
		assert(type_pool_.at(callable_id).is_function());
		// now we must create the function call type according to the function (due to labeled
		// arguments)
		auto const& function_arguments = type_pool_.at(callable_id).get_function().arguments;
		assert(function_arguments.size() == provided_arguments);
		std::vector<std::tuple<std::optional<std::string>, TypeInfo::ID>> arguments {};
		arguments.reserve(provided_arguments);
		std::transform(
			ordered_arguments.cbegin(),
			ordered_arguments.cend(),
			std::back_inserter(arguments),
			[](TypeInfo::ID argument) { return std::tuple {std::nullopt, argument}; }
		);
		for (size_t i = arguments.size(); i < function_arguments.size(); ++i) {
			std::optional<std::string> name = std::get<0>(function_arguments.at(i));
			// if it didn't have a name, it couldn't be a labeled argument
			assert(name.has_value());
			arguments.push_back({name, labeled_arguments.at(name.value())});
		}

		// we store the expression type as the return type so it automatically gets inferred!
		// TODO: once we have generics, we need to instantiate a copy of the function with unknowns
		TypeInfo function_call_type
			= TypeInfo::make_function(TypeInfo::Function {std::move(arguments), expr_type});
		UndecidedOverload::Candidate candidate {callable_id, function_call_type};
		candidates.push_back(std::move(candidate));
	}

	std::optional<AST::Identifier*> identifier = std::nullopt;

	if (function_call.callee->value.is_atom() || function_call.callee->value.get_atom().is_identifier())
		identifier = &function_call.callee->value.get_atom().get_identifier();

	UndecidedOverload overload {expr_type, identifier, std::move(candidates), std::move(rejections), span, file_id};

	if (!try_decide(overload)) undecided_overloads.push_back(std::move(overload));

	return expr_type;
}

Resolver::TypeInfo::ID
Resolver::infer(AST::Expression::MemberAccess& member_access, Span span, FileContext::ID file_id) {
	TypeInfo::ID accessee_id = infer(member_access.accessee->value, member_access.accessee->span, file_id);

	TypeInfo     type = TypeInfo::make_member_access(accessee_id, member_access.field.value);
	TypeInfo::ID id   = register_type(std::move(type), span, file_id);

	if (!try_decide(id)) undecided_member_accesses.push_back(id);

	return id;
}

Resolver::TypeInfo::ID Resolver::infer(AST::Expression& expression, Span span, FileContext::ID file_id) {
	switch (expression.kind()) {
	case AST::Expression::Kind::Atom:             expression.type = infer(expression.get_atom(), span, file_id); break;
	case AST::Expression::Kind::UnaryOperation:
	case AST::Expression::Kind::AddressOperation:
	case AST::Expression::Kind::BinaryOperation:  break;
	case AST::Expression::Kind::FunctionCall:
		expression.type = infer(expression.get_function_call(), span, file_id);
		break;
	case AST::Expression::Kind::MemberAccess:
		expression.type = infer(expression.get_member_access(), span, file_id);
		break;
	case AST::Expression::Kind::If: [[assume(false)]]; return expression.type.value();
	}

	// for operations, we turn them into function calls and then resolve them
	if (expression.is_unary_operation() || expression.is_binary_operation()) {
		bool is_unary = expression.is_unary_operation();

		Token::Symbol operator_ = is_unary ? expression.get_unary_operation().operation
		                                   : expression.get_binary_operation().operation;

		if (is_unary && operator_ == Token::Symbol::Star) {
			// the dereference operator cannot be overloaded either
			TypeInfo::ID argument
				= infer(expression.get_unary_operation().operand->value,
			                expression.get_unary_operation().operand->span,
			                file_id);
			if (!type_pool_.at(argument).is_pointer(type_pool_)) {
				parsed_files.at(file_id).diagnostics.push_back(
					Diagnostic::error(
						"type mismatch",
						"only pointers can be dereferenced",
						{get_type_sample(argument, OutFmt::Color::Red)}
					)
				);
				expression.type = register_type(TypeInfo::make_bottom(), span, file_id);

				return expression.type.value();
			}

			expression.type = type_pool_.at(argument).get_pointee(type_pool_);
		} else {
			// TODO: get the operator span
			Span operator_span = span;

			AST::Identifier callee_identifier {
				{operator_span, get_variant_name(operator_)}
			};
			callee_identifier.id   = get_operator_candidates(operator_, !is_unary);
			AST::Expression callee = AST::Expression::make_atom(
				AST::Expression::Atom::make_identifier(std::move(callee_identifier))
			);

			AST::Expression::FunctionCall::Arguments arguments {{}, {}};
			if (is_unary) arguments.ordered.push_back(std::move(*expression.get_unary_operation().operand));
			else {
				arguments.ordered.push_back(std::move(*expression.get_binary_operation().lhs));
				arguments.ordered.push_back(std::move(*expression.get_binary_operation().rhs));
			}

			expression = AST::Expression::make_function_call(
				std::make_unique<Spanned<AST::Expression>>(
					Spanned<AST::Expression> {operator_span, std::move(callee)}
				),
				std::move(arguments)
			);
			// TODO: change the diagnostics here to refer to operators and not functions
			expression.type = infer(expression.get_function_call(), span, file_id);
		}
	}

	// for address operations, we infer the inner type and deduce the type from there (no overloads are supported
	// for these)
	if (expression.is_address_operation()) {
		TypeInfo::ID inner
			= infer(expression.get_address_operation().operand->value,
		                expression.get_address_operation().operand->span,
		                file_id);
		expression.type = register_type(
			TypeInfo::make_pointer(TypeInfo::Pointer {inner, expression.get_address_operation().mutable_}),
			span,
			file_id
		);
	}

	return expression.type.value();
}

void Resolver::infer(AST::Statement::Declare& declare, FileContext::ID file_id) {
	if (!declare.value.has_value()) return;
	TypeInfo::ID variable_type = get_single_symbol(declare.name.value).type;
	TypeInfo::ID value_type    = infer(declare.value.value().value, declare.value.value().span, file_id);
	// we must make sure that the declared and actual type match
	unify(variable_type, value_type, file_id);
}

void Resolver::infer(AST::Statement::Set& set, FileContext::ID file_id) {
	TypeInfo::ID rhs_type = infer(set.rhs.value, set.rhs.span, file_id);
	// skip all invalid LHS
	if (!set.lhs.value.can_be_lhs()) return;
	if (set.lhs.value.is_atom()) {
		// identifiers
		if (!set.lhs.value.get_atom().is_identifier()) return;
		// if name resolution failed, we must move on
		if (!set.lhs.value.get_atom().get_identifier().id.has_value()) return;
		if (set.lhs.value.get_atom().get_identifier().id.value().empty()) return;
		// TODO: remove this, because set statements either have one lhs or most likely they should be forbidden
		if (set.lhs.value.get_atom().get_identifier().id.value().size() > 1) {
			std::cout
				<< "we gotta resolve the lhs for some reason? check whether nameres threw a diagnostic"
				<< std::endl;
			return;
		}
		TypeInfo::ID lhs_type = infer(set.lhs.value, set.lhs.span, file_id);
		unify(lhs_type, rhs_type, file_id);
	} else if (set.lhs.value.is_unary_operation()) {
		// derefs
		if (set.lhs.value.get_unary_operation().operation != Token::Symbol::Star) return;
		TypeInfo::ID lhs_type = infer(set.lhs.value, set.lhs.span, file_id);
		if (!type_pool_.at(lhs_type).is_pointer()) return;
		TypeInfo::Pointer const& pointer = type_pool_.at(lhs_type).get_pointer();
		unify(pointer.pointee, rhs_type, lhs_type, rhs_type, file_id);
	} else if (set.lhs.value.is_member_access()) {
		// member access is as simple as identifier lhs
		TypeInfo::ID lhs_type = infer(set.lhs.value, set.lhs.span, file_id);
		unify(lhs_type, rhs_type, file_id);
	}
}

void Resolver::infer(AST::Statement::Return& return_, Span span, AST::SymbolID function, FileContext::ID file_id) {
	TypeInfo::ID return_value = return_.value.has_value()
	                                  // if we do have a value, get its expression type
	                                  ? infer(return_.value.value().value, return_.value.value().span, file_id)
	                                  // if we don't, that's a return void
	                                  : register_type(TypeInfo::make_known_void(), span, file_id);
	// we must make sure that the return type and the returned value match
	unify(return_value, type_pool_.at(symbol_pool_.at(function).type).get_function().return_, file_id);
}

void Resolver::infer(Spanned<AST::Statement>& statement, AST::SymbolID function, FileContext::ID file_id) {
	switch (statement.value.kind()) {
	case AST::Statement::Kind::Declare:    infer(statement.value.get_declare(), file_id); return;
	case AST::Statement::Kind::Set:        infer(statement.value.get_set(), file_id); return;
	case AST::Statement::Kind::Expression: break;
	case AST::Statement::Kind::Return:
		infer(statement.value.get_return(), statement.span, function, file_id);
		return;
	case AST::Statement::Kind::Scope: infer(statement.value.get_scope(), function, file_id); return;
	case AST::Statement::Kind::Label:
	case AST::Statement::Kind::Goto:  return;
	case AST::Statement::Kind::Branch:
		// we need to infer the condition's type and unify it with bool
		infer(statement.value.get_branch().condition.value,
		      statement.value.get_branch().condition.span,
		      file_id);
		// FIXME: we need a better solution for built-in bool requirement
		unify(statement.value.get_branch().condition.value.type.value(),
		      register_type(TypeInfo::make_known_bool(), statement.value.get_branch().condition.span, file_id),
		      file_id);
		return;
	case AST::Statement::Kind::If: return;
	}

	// for expression statements, we want to throw a warning if it results in a non-void result
	TypeInfo::ID type_id = infer(statement.value.get_expression(), statement.span, file_id);
	if (type_pool_.at(type_id).is_bottom()) return;  // skip bottoms
	if (!can_unify(type_id, TypeInfo::make_known_void())) {
		parsed_files.at(file_id).diagnostics.push_back(
			Diagnostic::warning(
				"discarded expression result",
				"this expression's result is non-void",
				{get_type_sample(type_id, OutFmt::Color::Yellow)}
			)
		);
	}
}

void Resolver::infer(AST::Scope& scope, AST::SymbolID function, FileContext::ID file_id) {
	for (auto& statement : scope) { infer(statement, function, file_id); }
}

void Resolver::infer(AST::Function& function, FileContext::ID file_id) {
	if (function.body.has_value()) infer(function.body.value(), function.name.value.id.value()[0], file_id);
}

void Resolver::infer(AST::Module& module, FileContext::ID file_id) {
	for (Spanned<AST::Module::Item>& item : module.body.items) {
		auto& value = std::get<AST::Module::InnerItem>(item.value);
		if (std::holds_alternative<AST::Function>(value)) infer(std::get<AST::Function>(value), file_id);
		else if (std::holds_alternative<AST::Module>(value)) infer(std::get<AST::Module>(value), file_id);
	}
}

bool Resolver::try_decide_remaining_types() {
	// PERF: use std::copy_if
	while (!undecided_overloads.empty() || !undecided_member_accesses.empty()) {
		// we first try to decide the overloads
		std::vector<UndecidedOverload> remaining_overloads {};
		for (UndecidedOverload& undecided_overload : undecided_overloads)
			if (!try_decide(undecided_overload))
				remaining_overloads.push_back(std::move(undecided_overload));
		size_t old_size     = undecided_overloads.size();
		undecided_overloads = std::move(remaining_overloads);

		bool overloads_succeeded = old_size != undecided_overloads.size();

		// then we try to decide the member accesses
		std::vector<TypeInfo::ID> remaining_member_accesses {};
		for (TypeInfo::ID undecided_member_access : undecided_member_accesses)
			if (!try_decide(undecided_member_access))
				remaining_member_accesses.push_back(undecided_member_access);
		old_size                  = remaining_member_accesses.size();
		undecided_member_accesses = std::move(remaining_member_accesses);

		bool member_accesses_succeeded = old_size != undecided_member_accesses.size();

		// finally, we quit if we made no progress
		if (!overloads_succeeded && !member_accesses_succeeded) break;
	}

	// only if both arrays are empty we have finished deciding the program
	return undecided_overloads.empty() && undecided_member_accesses.empty();
}

void Resolver::decide_remaining_types() {
	// we try to decide remaining overloads and member accesses now that the entire program is known
	if (try_decide_remaining_types()) return;

	// we will try a more destructive approach now: we will fill in all partial numeric types and try to decide.
	// this doesn't really have an effect if it doesn't work, because the lowering phase would have done this
	// anyway!
	bool changes_made = false;
	for (UndecidedOverload& undecided_overload : undecided_overloads) {
		for (UndecidedOverload::Candidate& candidate : undecided_overload.candidates) {
			for (auto& [_, id] : candidate.call_type.get_function().arguments) {
				auto& type = type_pool_.at(id);
				if (type.is_partial_integer()) {
					auto& partial_integer = type.get_partial_integer();
					// we will fill in to a signed integer of default width
					bool signed_ = partial_integer.signed_is_known
					                     ? partial_integer.integer.is_signed()
					                     : true;
					std::optional<AST::Type::Atom::Integer> full_integer = std::nullopt;
					switch (partial_integer.integer.width_type()) {
					case AST::Type::Atom::Integer::WidthType::Fixed:
						full_integer = AST::Type::Atom::Integer::with_width(
								       partial_integer.integer.bit_width().value(),
								       signed_
						)
						                       .value();
						break;
					case AST::Type::Atom::Integer::WidthType::Any:
						full_integer = AST::Type::Atom::Integer::with_width(
							IR::DEFAULT_INTEGER_WIDTH,
							signed_
						);
						break;
					case AST::Type::Atom::Integer::WidthType::Ptr:
						full_integer = AST::Type::Atom::Integer::ptr(signed_);
						break;
					case AST::Type::Atom::Integer::WidthType::Size:
						full_integer = AST::Type::Atom::Integer::size(signed_);
						break;
					}
					type = TypeInfo::make_known_integer(
						TypeInfo::KnownInteger {std::move(full_integer.value())}
					);
					changes_made = true;
				} else if (type.is_partial_float()) {
					type = TypeInfo::make_known_float(
						TypeInfo::KnownFloat {
							(AST::Type::Atom::Float::Width) IR::DEFAULT_FLOAT_WIDTH
						}
					);
					changes_made = true;
				}
			}
		}
	}

	// if no type was filled in, there is no hope left
	if (changes_made) {
		// if any type was filled in, though, let's try again!
		if (try_decide_remaining_types()) return;
	}

	// if we did not manage to decide any overload, we gotta throw diagnostics (we literally tried everything we can
	// at this point :P)
	if (!undecided_overloads.empty()) {
		for (UndecidedOverload const& undecided_overload : undecided_overloads) {
			std::vector<Diagnostic::Sample> samples = {undecided_overload.rejections.at(0)};

			size_t count = 0;
			std::transform(
				undecided_overload.candidates.cbegin(),
				undecided_overload.candidates.cend(),
				std::back_inserter(samples),
				[this, &count](UndecidedOverload::Candidate const& candidate) {
					return Diagnostic::Sample(
						get_context(get_type_file_id(candidate.function)),
						std::format("candidate #{}", ++count),
						{Diagnostic::Sample::Label(
							get_type_span(candidate.function),
							OutFmt::Color::Cyan
						)}
					);
				}
			);

			parsed_files.at(undecided_overload.file_id)
				.diagnostics.push_back(
					Diagnostic::error(
						"could not resolve function overload",
						"more than one function matches the function call, so it must be manually disambiguated",
						std::move(samples)
					)
				);

			// we need to "unresolve" the callee identifier just in case
			if (undecided_overload.identifier.has_value()) undecided_overload.identifier.value()->id = {};
		}
	}

	if (!undecided_member_accesses.empty()) {
		for (TypeInfo::ID undecided_member_access : undecided_member_accesses) {
			// we need to determine what led to the member access to not be decided. there are two
			// possibilities
			TypeInfo::MemberAccess& member_access
				= type_pool_.at(undecided_member_access).get_member_access();
			Diagnostic::Sample sample  = get_type_sample(member_access.accessee, OutFmt::Color::Red);
			FileContext::ID    file_id = get_type_file_id(undecided_member_access);

			// the first is that there is not a single underlying type
			if (!type_pool_.at(member_access.accessee).get_single_underlying(type_pool_).has_value()) {
				parsed_files.at(file_id).diagnostics.push_back(
					Diagnostic::error(
						"could not resolve member access",
						"the underlying type of the accessee could not be decided",
						{std::move(sample)}
					)
				);
				continue;
			}

			// the second is that the type cannot have fields
			parsed_files.at(file_id).diagnostics.push_back(
				Diagnostic::error(
					"could not resolve member access",
					"the underlying type of the accessee does not have any fields",
					{std::move(sample)}
				)
			);
		}
	}
}

void Resolver::infer_types() {
	for (ParsedFile& file : parsed_files) { infer(file.module, file.file_id); }
	decide_remaining_types();
}
