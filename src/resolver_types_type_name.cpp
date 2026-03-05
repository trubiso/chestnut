#include "resolver.hpp"

#include <cstdlib>
#include <iostream>
#include <sstream>
#include <variant>

std::ostream& Resolver::debug_print_type(std::ostream& os, TypeInfo const& type) const {
	switch (type.kind()) {
	case TypeInfo::Kind::Unknown:        return os << "(unknown type)";
	case TypeInfo::Kind::Bottom:         return os << "(bottom)";
	case TypeInfo::Kind::Module:         return os << "(module)";
	case TypeInfo::Kind::KnownVoid:      return os << "void";
	case TypeInfo::Kind::KnownChar:      return os << "char";
	case TypeInfo::Kind::KnownBool:      return os << "bool";
	case TypeInfo::Kind::PartialFloat:   return os << "(float)";
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
		os << "(function with generics <";
		size_t count = 0;
		for (auto const& [name, generic] : function.generics) {
			os << (name.has_value() ? name.value() : "(anonymous)") << ": ";
			debug_print_type(os, generic);
			if (++count < function.arguments.size()) os << ", ";
		}
		os << ">, args (";
		count = 0;
		for (auto const& [name, arg_type] : function.arguments) {
			os << (name.has_value() ? name.value() : "(anonymous)") << ": ";
			debug_print_type(os, arg_type);
			if (++count < function.arguments.size()) os << ", ";
		}
		os << ") and return type ";
		return debug_print_type(os, function.return_) << ")";
	} else if (type.is_same_as()) {
		os << "=(";
		size_t count = 0;
		for (TypeInfo::ID subid : type.get_same_as().ids) {
			debug_print_type(os, subid);
			if (++count < type.get_same_as().ids.size()) os << " | ";
		}
		return os << ")";
	} else if (type.is_generic()) {
		TypeInfo::Generic const& generic = type.get_generic();
		os << symbol_pool_.at(generic.name).name;
		if (!generic.declared_constraints.empty()) {
			os << ": ";
			size_t count = 0;
			for (auto const& constraint : generic.declared_constraints) {
				os << symbol_pool_.at(constraint.name).name;
				if (!constraint.arguments.empty()) {
					os << '<';
					size_t subcount = 0;
					for (TypeInfo::ID subtype : constraint.arguments) {
						debug_print_type(os, subtype);
						if (++subcount < constraint.arguments.size()) os << ", ";
					}
					os << '>';
				}
				if (++count < generic.declared_constraints.size()) os << " + ";
			}
		}
		if (!generic.imposed_constraints.empty()) {
			os << " (imposed: ";
			size_t count = 0;
			for (auto const& constraint : generic.imposed_constraints) {
				if (std::holds_alternative<TypeInfo::Generic::TraitConstraint>(constraint)) {
					auto const& trait_constraint
						= std::get<TypeInfo::Generic::TraitConstraint>(constraint);
					os << symbol_pool_.at(trait_constraint.name).name;
					if (!trait_constraint.arguments.empty()) {
						os << '<';
						size_t subcount = 0;
						for (TypeInfo::ID subtype : trait_constraint.arguments) {
							debug_print_type(os, subtype);
							if (++subcount < trait_constraint.arguments.size()) os << ", ";
						}
						os << '>';
					}
				} else
					debug_print_type(
						os,
						std::get<TypeInfo::Generic::TypeConstraint>(constraint).type
					);
				if (++count < generic.imposed_constraints.size()) os << " + ";
			}
			os << ')';
		}

		return os;
	} else if (type.is_member_access()) {
		TypeInfo::MemberAccess const& member_access = type.get_member_access();
		debug_print_type(os, member_access.accessee) << '.' << member_access.field;
		if (!member_access.possible_types.empty()) {
			os << " (possibly: ";
			size_t count = 0;
			for (TypeInfo::ID subid : member_access.possible_types) {
				debug_print_type(os, subid);
				if (++count < member_access.possible_types.size()) os << ", ";
			}
			os << ')';
		}
		return os;
	} else if (type.is_named()) {
		TypeInfo::Named const& named = type.get_named();

		if (named.is_partial()) {
			auto const& partial = std::get<TypeInfo::Named::Partial>(named.value);
			os << *named.name;

			size_t generic_count = partial.ordered_generics.size() + partial.labeled_generics.size();
			if (generic_count == 0) return os;

			os << '<';
			size_t count = 0;
			for (TypeInfo::ID generic : partial.ordered_generics) {
				debug_print_type(os, generic);
				if (++count < generic_count) os << ", ";
			}
			for (auto const& [label, generic] : partial.labeled_generics) {
				os << label << ": ";
				debug_print_type(os, generic);
				if (++count < generic_count) os << ", ";
			}
			os << '>';
			return os;
		}

		if (named.candidates().empty()) return os << "(impossible)";
		if (named.candidates().size() > 1) os << '(';
		size_t count = 0;
		for (auto const& candidate : named.candidates()) {
			os << symbol_pool_.at(candidate.name).name;

			if (!candidate.generics.empty()) {
				os << '<';
				size_t subcount = 0;
				for (TypeInfo::ID id : candidate.generics) {
					debug_print_type(os, id);
					if (++subcount < candidate.generics.size()) os << ", ";
				}
				os << '>';
			}

			if (++count < named.candidates().size()) os << " | ";
		}
		if (named.candidates().size() > 1) os << ')';
		return os;
	} else if (type.is_pointer()) {
		TypeInfo::Pointer const& pointer = type.get_pointer();
		os << "(*" << (pointer.mutable_ ? "mut" : "const") << " ";
		return debug_print_type(os, pointer.pointee) << ")";
	} else if (type.is_known_integer()) {
		AST::Type::Atom::Integer integer = type.get_known_integer().integer;
		return os << AST::Type::Atom::make_integer(std::move(integer));
	} else if (type.is_known_float()) {
		return os << AST::Type::Atom::make_float(type.get_known_float().width);
	} else if (type.is_partial_integer()) {
		AST::Type::Atom::Integer integer = type.get_partial_integer().integer;

		bool signed_is_known = type.get_partial_integer().signed_is_known;

		os << (signed_is_known ? (integer.is_signed() ? "" : "u") : "?") << "int";
		switch (integer.width_type()) {
		case AST::Type::Atom::Integer::WidthType::Fixed: return os << integer.bit_width().value();
		case AST::Type::Atom::Integer::WidthType::Any:   return os;
		case AST::Type::Atom::Integer::WidthType::Ptr:   return os << "ptr";
		case AST::Type::Atom::Integer::WidthType::Size:  return os << "size";
		}
	}

	[[assume(false)]];
	return os;
}

std::ostream& Resolver::debug_print_type(std::ostream& os, TypeInfo::ID id) const {
	os << "$" << id << " ";
	return debug_print_type(os, type_pool_.at(id));
}

void Resolver::debug_print_type(TypeInfo::ID id) const {
	std::cout << "$" << id << " ";
	debug_print_type(type_pool_.at(id));
}

void Resolver::debug_print_type(TypeInfo const& type) const {
	debug_print_type(std::cout, type);
}

std::ostream& Resolver::get_type_name(std::ostream& os, TypeInfo const& type) const {
	switch (type.kind()) {
	case TypeInfo::Kind::Unknown:        return os << "unknown";
	case TypeInfo::Kind::Bottom:         return os << "bottom";
	case TypeInfo::Kind::Module:         return os << "module";
	case TypeInfo::Kind::KnownVoid:      return os << "void";
	case TypeInfo::Kind::KnownChar:      return os << "char";
	case TypeInfo::Kind::KnownBool:      return os << "bool";
	case TypeInfo::Kind::PartialFloat:   return os << "float";
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
		// TODO: print generics
		os << "func(";
		size_t count = 0;
		for (auto const& [name, arg_type] : function.arguments) {
			os << (name.has_value() ? name.value() : "_") << ": ";
			get_type_name(os, arg_type);
			if (++count < function.arguments.size()) os << ", ";
		}
		os << ") ";
		return get_type_name(os, function.return_) << "";
	} else if (type.is_same_as()) {
		if (type.get_same_as().ids.size() > 1) os << '(';
		size_t count = 0;
		for (TypeInfo::ID subid : type.get_same_as().ids) {
			get_type_name(os, subid);
			if (++count < type.get_same_as().ids.size()) os << " | ";
		}
		if (type.get_same_as().ids.size() > 1) os << ')';
		return os;
	} else if (type.is_generic()) {
		TypeInfo::Generic const& generic = type.get_generic();
		os << symbol_pool_.at(generic.name).name;
		if (!generic.declared_constraints.empty()) {
			os << ": ";
			size_t count = 0;
			for (auto const& constraint : generic.declared_constraints) {
				os << symbol_pool_.at(constraint.name).name;
				if (!constraint.arguments.empty()) {
					os << '<';
					size_t subcount = 0;
					for (TypeInfo::ID subtype : constraint.arguments) {
						get_type_name(os, subtype);
						if (++subcount < constraint.arguments.size()) os << ", ";
					}
					os << '>';
				}
				if (++count < generic.declared_constraints.size()) os << " + ";
			}
		}
		return os;
	} else if (type.is_member_access()) {
		TypeInfo::MemberAccess const& member_access = type.get_member_access();
		return get_type_name(os, member_access.accessee) << '.' << member_access.field;
	} else if (type.is_named()) {
		TypeInfo::Named const& named = type.get_named();
		// we shouldn't be calling this function pre-partial pruning
		assert(!named.is_partial());

		if (named.candidates().empty()) return os << "(impossible)";
		if (named.candidates().size() > 1) os << '(';
		size_t count = 0;
		for (auto const& candidate : named.candidates()) {
			os << symbol_pool_.at(candidate.name).name;

			if (!candidate.generics.empty()) {
				os << '<';
				size_t subcount = 0;
				for (TypeInfo::ID id : candidate.generics) {
					get_type_name(os, id);
					if (++subcount < candidate.generics.size()) os << ", ";
				}
				os << '>';
			}

			if (++count < named.candidates().size()) os << " | ";
		}
		if (named.candidates().size() > 1) os << ')';
		return os;
	} else if (type.is_pointer()) {
		TypeInfo::Pointer const& pointer = type.get_pointer();
		os << "*" << (pointer.mutable_ ? "mut" : "const") << " ";
		return get_type_name(os, pointer.pointee);
	} else if (type.is_known_integer()) {
		AST::Type::Atom::Integer integer = type.get_known_integer().integer;
		return os << AST::Type::Atom::make_integer(std::move(integer));
	} else if (type.is_known_float()) {
		return os << AST::Type::Atom::make_float(type.get_known_float().width);
	} else if (type.is_partial_integer()) {
		AST::Type::Atom::Integer integer = type.get_partial_integer().integer;

		bool signed_is_known = type.get_partial_integer().signed_is_known;

		os << (signed_is_known ? (integer.is_signed() ? "" : "u") : "(u)") << "int";
		switch (integer.width_type()) {
		case AST::Type::Atom::Integer::WidthType::Fixed: return os << integer.bit_width().value();
		case AST::Type::Atom::Integer::WidthType::Any:   return os;
		case AST::Type::Atom::Integer::WidthType::Ptr:   return os << "ptr";
		case AST::Type::Atom::Integer::WidthType::Size:  return os << "size";
		}
	}

	[[assume(false)]];
	return os;
}

std::ostream& Resolver::get_type_name(std::ostream& os, TypeInfo::ID id) const {
	return get_type_name(os, type_pool_.at(id));
}

std::string Resolver::get_type_name(TypeInfo const& type) const {
	std::stringstream output {};
	get_type_name(output, type);
	return output.str();
}

std::string Resolver::get_type_name(TypeInfo::ID id) const {
	return get_type_name(type_pool_.at(id));
}
