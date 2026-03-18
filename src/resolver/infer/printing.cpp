#include "resolver.hpp"

#include <iostream>
#include <sstream>
#include <utility>

std::ostream& Resolver::debug_print(std::ostream& os, TypeVar const& type) const {
	switch (type.kind()) {
	case TypeVar::Kind::Unknown: return os << "(unknown type)";
	case TypeVar::Kind::Bottom:  return os << "(bottom)";
	case TypeVar::Kind::Module:  return os << "(module)";
	case TypeVar::Kind::Named:   {
		TypeVar::Named const& named = type.get_named();
		debug_print_name(os, named.name);
		return os;
	}
	case TypeVar::Kind::Pointer: {
		TypeVar::Pointer const& pointer = type.get_pointer();
		os << "(*" << (pointer.mutable_ ? "mut" : "const") << " ";
		return debug_print_type(os, pointer.pointee) << ")";
	}
	case TypeVar::Kind::Void:    return os << "void";
	case TypeVar::Kind::Char:    return os << "char";
	case TypeVar::Kind::Bool:    return os << "bool";
	case TypeVar::Kind::Integer: {
		AST::Type::Atom::Integer integer = type.get_integer().integer;
		return os << AST::Type::Atom::make_integer(std::move(integer));
	}
	case TypeVar::Kind::Float:          return os << AST::Type::Atom::make_float(type.get_float().width);
	case TypeVar::Kind::PartialInteger: {
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
	case TypeVar::Kind::PartialFloat: return os << "(float)";
	}
	std::unreachable();
}

void Resolver::debug_print(TypeVar const& type) const {
	debug_print(std::cout, type);
}

std::ostream& Resolver::debug_print_type(std::ostream& os, TypeVar::ID id) const {
	os << "$" << id << " ";
	return debug_print(os, get_type_var(id));
}

void Resolver::debug_print_type(TypeVar::ID id) const {
	std::cout << "$" << id << " ";
	debug_print(get_type_var(id));
}

std::ostream& Resolver::get_name(std::ostream& os, TypeVar const& type) const {
	switch (type.kind()) {
	case TypeVar::Kind::Unknown: return os << "unknown";
	case TypeVar::Kind::Bottom:  return os << "bottom";
	case TypeVar::Kind::Module:  return os << "module";
	case TypeVar::Kind::Named:   {
		TypeVar::Named const& named = type.get_named();
		get_name_name(os, named.name);
		return os;
	}
	case TypeVar::Kind::Pointer: {
		TypeVar::Pointer const& pointer = type.get_pointer();
		os << "*" << (pointer.mutable_ ? "mut" : "const") << " ";
		return get_type_name(os, pointer.pointee);
	}
	case TypeVar::Kind::Void:    return os << "void";
	case TypeVar::Kind::Char:    return os << "char";
	case TypeVar::Kind::Bool:    return os << "bool";
	case TypeVar::Kind::Integer: {
		AST::Type::Atom::Integer integer = type.get_integer().integer;
		return os << AST::Type::Atom::make_integer(std::move(integer));
	}
	case TypeVar::Kind::Float:          return os << AST::Type::Atom::make_float(type.get_float().width);
	case TypeVar::Kind::PartialInteger: {
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
	case TypeVar::Kind::PartialFloat: return os << "float";
	}
	std::unreachable();
}

std::string Resolver::get_name(TypeVar const& type) const {
	std::stringstream output {};
	get_name(output, type);
	return output.str();
}

std::ostream& Resolver::get_type_name(std::ostream& os, TypeVar::ID id) const {
	return get_name(os, get_type_var(id));
}

std::string Resolver::get_type_name(TypeVar::ID id) const {
	return get_name(get_type_var(id));
}
