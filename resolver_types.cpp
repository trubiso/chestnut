#include "levenshtein.hpp"
#include "resolver.hpp"

#include <cstdlib>
#include <iostream>
#include <sstream>
#include <variant>

Resolver::TypeInfo Resolver::TypeInfo::from_type(AST::Type::Atom const& atom) {
	switch (atom.kind()) {
	case AST::Type::Atom::Kind::Float:    return make_known_float(KnownFloat {atom.get_float().width});
	case AST::Type::Atom::Kind::Void:     return make_known_void();
	case AST::Type::Atom::Kind::Char:     return make_known_char();
	case AST::Type::Atom::Kind::Bool:     return make_known_bool();
	case AST::Type::Atom::Kind::Inferred: return make_unknown();
	case AST::Type::Atom::Kind::Integer:  break;
	}

	// for integers, we need to determine how much information we know
	AST::Type::Atom::Integer const& integer = atom.get_integer();
	if (integer.width_type() != AST::Type::Atom::Integer::WidthType::Any)
		return make_known_integer(KnownInteger {integer});
	else return make_partial_integer(PartialInteger {integer, true});
}

Resolver::TypeInfo Resolver::TypeInfo::from_type(AST::Type const& type) {
	switch (type.kind()) {
	case AST::Type::Kind::Atom: return from_type(type.get_atom());
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
		for (TypeInfo::ID subid : subids) ids.push_back(subid);
	}

	return ids;
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
	case TypeInfo::Kind::KnownInteger:
	case TypeInfo::Kind::KnownFloat:
	case TypeInfo::Kind::PartialInteger: break;
	}

	if (type.kind() == TypeInfo::Kind::Function) {
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
	} else if (type.kind() == TypeInfo::Kind::SameAs) {
		std::cout << "=(";
		size_t count = 0;
		for (TypeInfo::ID subid : type.get_same_as().ids) {
			debug_print_type(subid);
			if (++count < type.get_same_as().ids.size()) std::cout << " | ";
		}
		std::cout << ")";
	} else if (type.kind() == TypeInfo::Kind::KnownInteger) {
		AST::Type::Atom::Integer integer = type.get_known_integer().integer;
		std::cout << AST::Type::Atom::make_integer(std::move(integer));
	} else if (type.kind() == TypeInfo::Kind::KnownFloat) {
		std::cout << AST::Type::Atom::make_float(type.get_known_float().width);
	} else if (type.kind() == TypeInfo::Kind::PartialInteger) {
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
	case TypeInfo::Kind::KnownInteger:
	case TypeInfo::Kind::KnownFloat:
	case TypeInfo::Kind::PartialInteger: break;
	}

	std::stringstream output {};
	if (type.kind() == TypeInfo::Kind::Function) {
		TypeInfo::Function const& function = type.get_function();
		output << "func(";
		size_t count = 0;
		for (auto const& [name, arg_type] : function.arguments) {
			output << (name.has_value() ? name.value() : "_") << ": ";
			output << get_type_name(arg_type);
			if (++count < function.arguments.size()) output << ", ";
		}
		output << ") " << get_type_name(function.return_) << "";
	} else if (type.kind() == TypeInfo::Kind::SameAs) {
		if (type.get_same_as().ids.size() > 1) output << '(';
		size_t count = 0;
		for (TypeInfo::ID subid : type.get_same_as().ids) {
			output << get_type_name(subid);
			if (++count < type.get_same_as().ids.size()) output << " | ";
		}
		if (type.get_same_as().ids.size() > 1) output << ')';
	} else if (type.kind() == TypeInfo::Kind::KnownInteger) {
		AST::Type::Atom::Integer integer = type.get_known_integer().integer;
		output << AST::Type::Atom::make_integer(std::move(integer));
	} else if (type.kind() == TypeInfo::Kind::KnownFloat) {
		output << AST::Type::Atom::make_float(type.get_known_float().width);
	} else if (type.kind() == TypeInfo::Kind::PartialInteger) {
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
		if (type_pool_.at(symbol.type).kind() != TypeInfo::Kind::Function) continue;
		if (type_pool_.at(symbol.type).get_function().arguments.size() != (binary ? 2 : 1)) continue;
		symbols.push_back(symbol.id);
	}
	return symbols;
}

void Resolver::set_same_as(TypeInfo::ID to, TypeInfo::ID from) {
	// set_same_as(a, a) is a noop
	if (to == from) return;
	// the normal case works as always
	if (type_pool_.at(from).kind() != TypeInfo::Kind::SameAs) {
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
	assert(type_pool_.at(same_as).kind() == TypeInfo::Kind::SameAs);
	std::vector<TypeInfo::ID> const& ids = type_pool_.at(same_as).get_same_as().ids;

	// if we have a single id, unify it as normal
	if (ids.size() == 1) {
		unify(ids[0], other, same_as_origin, other_origin, file_id);
		return TypeInfo::make_same_as(ids[0]);
	}

	// if we have more than one, filter the non-unifiable ones out
	std::vector<TypeInfo::ID> new_ids {};
	for (TypeInfo::ID id : ids)
		if (can_unify(id, other)) new_ids.push_back(id);

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

void Resolver::unify_functions(
	TypeInfo::ID    function,
	TypeInfo::ID    other,
	TypeInfo::ID    function_origin,
	TypeInfo::ID    other_origin,
	FileContext::ID file_id
) {
	// ensure they're both functions
	assert(type_pool_.at(function).kind() == TypeInfo::Kind::Function);
	if (type_pool_.at(other).kind() != TypeInfo::Kind::Function) {
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

void Resolver::unify(
	TypeInfo::ID    a_id,
	TypeInfo::ID    b_id,
	TypeInfo::ID    a_origin,
	TypeInfo::ID    b_origin,
	FileContext::ID file_id
) {
	TypeInfo &a = type_pool_.at(a_id), &b = type_pool_.at(b_id);

	// follow references
	if (a.kind() == TypeInfo::Kind::SameAs) {
		a = unify_follow_references(a_id, b_id, a_origin, b_origin, file_id);
		return;
	}
	if (b.kind() == TypeInfo::Kind::SameAs) {
		b = unify_follow_references(b_id, a_id, b_origin, a_origin, file_id);
		return;
	}

	// bottoms don't participate in unification
	if (a.kind() == TypeInfo::Kind::Bottom || b.kind() == TypeInfo::Kind::Bottom) return;

	// make unknowns known
	if (a.kind() == TypeInfo::Kind::Unknown) {
		set_same_as(a_id, b_id);
		return;
	}
	if (b.kind() == TypeInfo::Kind::Unknown) {
		set_same_as(b_id, a_id);
		return;
	}

	// if any of them is a basic Known type, the other must be exactly the same
	if (unify_basic_known(TypeInfo::Kind::KnownVoid, a_id, b_id, a_origin, b_origin, file_id)) return;
	if (unify_basic_known(TypeInfo::Kind::KnownChar, a_id, b_id, a_origin, b_origin, file_id)) return;
	if (unify_basic_known(TypeInfo::Kind::KnownBool, a_id, b_id, a_origin, b_origin, file_id)) return;

	// modules act like basic Known types right now, but this is silly.
	// TODO: do something better
	if (unify_basic_known(TypeInfo::Kind::Module, a_id, b_id, a_origin, b_origin, file_id)) return;

	// functions
	if (a.kind() == TypeInfo::Kind::Function) return unify_functions(a_id, b_id, a_origin, b_origin, file_id);
	if (b.kind() == TypeInfo::Kind::Function) return unify_functions(b_id, a_id, b_origin, a_origin, file_id);

	// now only numeric types are left ([Known/Partial][Integer/Float])
	bool a_known = a.kind() == TypeInfo::Kind::KnownInteger || a.kind() == TypeInfo::Kind::KnownFloat,
	     b_known = b.kind() == TypeInfo::Kind::KnownInteger || b.kind() == TypeInfo::Kind::KnownFloat;
	bool a_float = a.kind() == TypeInfo::Kind::KnownFloat || a.kind() == TypeInfo::Kind::PartialFloat,
	     b_float = b.kind() == TypeInfo::Kind::KnownFloat || b.kind() == TypeInfo::Kind::PartialFloat;

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
	assert(same_as.kind() == TypeInfo::Kind::SameAs);
	std::vector<TypeInfo::ID> const& ids = same_as.get_same_as().ids;
	for (TypeInfo::ID id : ids)
		if (can_unify(id, other)) return true;
	return false;
}

std::optional<bool> Resolver::can_unify_basic_known(TypeInfo::Kind kind, TypeInfo const& a, TypeInfo const& b) const {
	bool a_matches = a.kind() == kind, b_matches = b.kind() == kind;
	if (!a_matches && !b_matches) return std::nullopt;  // if neither match, this case is not for us
	if (a_matches && b_matches) return true;            // if both match, this is a freebie
	return false;                                       // if neither match, it won't work
}

bool Resolver::can_unify_functions(TypeInfo const& function, TypeInfo const& other) const {
	// ensure they're both functions
	assert(function.kind() == TypeInfo::Kind::Function);
	if (other.kind() != TypeInfo::Kind::Function) { return false; }

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
	if (a.kind() == TypeInfo::Kind::SameAs) return can_unify_follow_references(a, b);
	if (b.kind() == TypeInfo::Kind::SameAs) return can_unify_follow_references(b, a);

	// bottoms don't participate in unification
	if (a.kind() == TypeInfo::Kind::Bottom || b.kind() == TypeInfo::Kind::Bottom) return true;

	// make unknowns known
	if (a.kind() == TypeInfo::Kind::Unknown) return true;
	if (b.kind() == TypeInfo::Kind::Unknown) return true;

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
	if (a.kind() == TypeInfo::Kind::Function) return can_unify_functions(a, b);
	if (b.kind() == TypeInfo::Kind::Function) return can_unify_functions(b, a);

	// now only numeric types are left ([Known/Partial][Integer/Float])
	bool a_known = a.kind() == TypeInfo::Kind::KnownInteger || a.kind() == TypeInfo::Kind::KnownFloat,
	     b_known = b.kind() == TypeInfo::Kind::KnownInteger || b.kind() == TypeInfo::Kind::KnownFloat;
	bool a_float = a.kind() == TypeInfo::Kind::KnownFloat || a.kind() == TypeInfo::Kind::PartialFloat,
	     b_float = b.kind() == TypeInfo::Kind::KnownFloat || b.kind() == TypeInfo::Kind::PartialFloat;

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

Resolver::TypeInfo::ID Resolver::infer(AST::Expression::Atom const& atom, Span span, FileContext::ID file_id) {
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
	case AST::Expression::Atom::Kind::Expression: return infer(*atom.get_expression(), span, file_id);
	case AST::Expression::Atom::Kind::Identifier: break;
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
	for (AST::SymbolID id : ids) { type_ids.push_back(get_single_symbol(id).type); }
	if (type_ids.size() == 1) return register_type(TypeInfo::make_same_as(type_ids[0]), span, file_id);
	else return register_type(TypeInfo::make_same_as(std::move(type_ids)), span, file_id);
}

Resolver::TypeInfo::ID
Resolver::infer(AST::Expression::FunctionCall& function_call, Span span, FileContext::ID file_id) {
	// for function calls, we need to resolve or partially resolve the overload
	TypeInfo::ID callee_id = infer(function_call.callee->value, function_call.callee->span, file_id);

	// first, we ensure that there is at least one callable item
	if (!type_pool_.at(callee_id).is_callable(type_pool_)) {
		if (type_pool_.at(callee_id).kind() != TypeInfo::Kind::Bottom)
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
		assert(type_pool_.at(callable_id).kind() == TypeInfo::Kind::Function);
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
				auto const& identifier      = std::get<0>(argument);
				bool        argument_exists = false;
				for (std::string_view actual_argument : arguments_under_consideration) {
					if (actual_argument == identifier.value.name()) {
						argument_exists = true;
						break;
					}
				}
				if (!argument_exists) {
					std::stringstream text {};
					text << "missing labeled argument '" << identifier.value.name() << "'";
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
		if (function_call.callee->value.kind() == AST::Expression::Kind::Atom
		    || function_call.callee->value.get_atom().kind() == AST::Expression::Atom::Kind::Identifier) {
			function_call.callee->value.get_atom().get_identifier().id = {};
		}
		return register_type(TypeInfo::make_bottom(), span, file_id);
	}

	// it's time to infer the arguments now that we have a set of possible callees
	std::vector<TypeInfo::ID> ordered_arguments {};
	ordered_arguments.reserve(function_call.arguments.ordered.size());
	for (auto& argument : function_call.arguments.ordered) {
		ordered_arguments.push_back(infer(argument.value, argument.span, file_id));
	}

	std::unordered_map<std::string, TypeInfo::ID> labeled_arguments {};
	ordered_arguments.reserve(function_call.arguments.labeled.size());
	for (auto& [identifier, value] : function_call.arguments.labeled) {
		labeled_arguments.emplace(identifier.value.name(), infer(value.value, value.span, file_id));
	}

	TypeInfo::ID return_ = register_type(TypeInfo::make_unknown(), span, file_id);

	// we will filter the functions based on whether they are unifiable
	std::vector<TypeInfo::ID> found_functions {};
	std::vector<TypeInfo>     function_call_types {};
	for (TypeInfo::ID callable_id : callable_filtered) {
		assert(type_pool_.at(callable_id).kind() == TypeInfo::Kind::Function);
		// now we must create the function call type according to the function (due to labeled
		// arguments)
		auto const& function_arguments = type_pool_.at(callable_id).get_function().arguments;
		assert(function_arguments.size() == provided_arguments);
		std::vector<std::tuple<std::optional<std::string>, TypeInfo::ID>> arguments {};
		arguments.reserve(provided_arguments);
		for (TypeInfo::ID argument : ordered_arguments) arguments.push_back({std::nullopt, argument});
		for (size_t i = arguments.size(); i < function_arguments.size(); ++i) {
			std::optional<std::string> name = std::get<0>(function_arguments.at(i));
			// if it didn't have a name, it couldn't be a labeled argument
			assert(name.has_value());
			arguments.push_back({name, labeled_arguments.at(name.value())});
		}

		// TODO: once we have generics, we need to instantiate a copy of the function with unknowns
		TypeInfo function_call_type
			= TypeInfo::make_function(TypeInfo::Function {std::move(arguments), return_});
		if (can_unify(function_call_type, callable_id)) {
			found_functions.push_back(callable_id);
			function_call_types.push_back(function_call_type);
		} else {
			// TODO: specify how it is incompatible?
			std::stringstream text {};
			text
				<< "function signature ("
				<< get_type_name(callable_id)
				<< ") is incompatible with the function call signature ("
				<< get_type_name(function_call_type)
				<< ")";
			rejections.push_back(
				Diagnostic::Sample(
					get_context(get_type_file_id(callable_id)),
					{Diagnostic::Sample::Label(
						get_type_span(callable_id),
						text.str(),
						OutFmt::Color::Magenta
					)}
				)
			);
		}
	}

	// now, we only have unifiable functions left. if none are unifiable, it's unresolved.
	if (found_functions.empty()) {
		parsed_files.at(file_id).diagnostics.push_back(
			Diagnostic::error(
				"could not resolve function overload",
				"no function matched the constraints imposed by the function call",
				std::move(rejections)
			)
		);
		// we need to "unresolve" the callee identifier just in case
		if (function_call.callee->value.kind() == AST::Expression::Kind::Atom
		    || function_call.callee->value.get_atom().kind() == AST::Expression::Atom::Kind::Identifier) {
			function_call.callee->value.get_atom().get_identifier().id = {};
		}
		return register_type(TypeInfo::make_bottom(), span, file_id);
	}

	// if too many are unifiable, we unify with none and throw a diagnostic. it is more sensible to do it this way
	// than to unify with the first one we see!
	if (found_functions.size() > 1) {
		std::vector<Diagnostic::Sample> samples = {rejections.at(0)};

		size_t count = 0;
		for (TypeInfo::ID id : found_functions)
			samples.push_back(
				Diagnostic::Sample(
					get_context(get_type_file_id(id)),
					std::format("candidate #{}", ++count),
					{Diagnostic::Sample::Label(get_type_span(id), OutFmt::Color::Cyan)}
				)
			);

		parsed_files.at(file_id).diagnostics.push_back(
			Diagnostic::error(
				"could not resolve function overload",
				"more than one function matches the function call, so it must be manually disambiguated",
				std::move(samples)
			)
		);
		return register_type(TypeInfo::make_bottom(), span, file_id);
	}

	// if only one is unifiable, we've finally found the one and only function
	assert(found_functions.size() == 1 && function_call_types.size() == 1);
	TypeInfo::ID call_id = register_type(std::move(function_call_types.at(0)), span, file_id);
	unify(call_id, found_functions.at(0), file_id);

	// if we're calling an identifier, let's finish resolving it
	if (function_call.callee->value.kind() == AST::Expression::Kind::Atom
	    || function_call.callee->value.get_atom().kind() == AST::Expression::Atom::Kind::Identifier) {
		if (!type_symbol_mapping_.at(found_functions[0]).has_value()) {
			// TODO: think about when this would ever happen
			std::cout << "there is no value for this call id what?: ";
			debug_print_type(found_functions[0]);
			std::cout << std::endl;
		} else {
			function_call.callee->value.get_atom().get_identifier().id
				= {type_symbol_mapping_.at(found_functions[0]).value()};
		}
	}

	return register_type(TypeInfo::make_same_as(type_pool_.at(call_id).get_function().return_), span, file_id);
}

Resolver::TypeInfo::ID Resolver::infer(AST::Expression& expression, Span span, FileContext::ID file_id) {
	switch (expression.kind()) {
	case AST::Expression::Kind::Atom:            expression.type = infer(expression.get_atom(), span, file_id); break;
	case AST::Expression::Kind::UnaryOperation:
	case AST::Expression::Kind::BinaryOperation: break;
	case AST::Expression::Kind::FunctionCall:
		expression.type = infer(expression.get_function_call(), span, file_id);
		break;
	}

	// for operations, we turn them into function calls and then resolve them
	if (expression.kind() == AST::Expression::Kind::UnaryOperation
	    || expression.kind() == AST::Expression::Kind::BinaryOperation) {
		// TODO: get the operator span
		Span operator_span = span;

		bool is_unary = expression.kind() == AST::Expression::Kind::UnaryOperation;

		Token::Symbol operator_ = is_unary ? expression.get_unary_operation().operation
		                                   : expression.get_binary_operation().operation;

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
	// skip all invalid LHS
	if (!set.lhs.value.can_be_lhs()) return;
	// we can only deal with identifiers rn
	if (set.lhs.value.get_atom().kind() != AST::Expression::Atom::Kind::Identifier) return;
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
	// lhs and rhs must have the same type
	Symbol const& lhs      = symbol_pool_.at(set.lhs.value.get_atom().get_identifier().id.value().at(0));
	TypeInfo ::ID rhs_type = infer(set.rhs.value, set.rhs.span, file_id);
	unify(lhs.type, rhs_type, file_id);
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
	}

	// for expression statements, we want to throw a warning if it results in a non-void result
	TypeInfo::ID type_id = infer(statement.value.get_expression(), statement.span, file_id);
	if (type_pool_.at(type_id).kind() == TypeInfo::Kind::Bottom) return;  // skip bottoms
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

void Resolver::infer_types() {
	for (ParsedFile& file : parsed_files) { infer(file.module, file.file_id); }
}
