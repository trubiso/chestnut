#include "type.hpp"

namespace AST {

std::ostream& operator<<(std::ostream& os, Type::Atom const& atom) {
	switch (atom.kind()) {
	case Type::Atom::Kind::Integer:  break;
	case Type::Atom::Kind::Float:    return os << "float" << (uint32_t) atom.get_float().width_value();
	case Type::Atom::Kind::Void:     return os << "void";
	case Type::Atom::Kind::Char:     return os << "char";
	case Type::Atom::Kind::Bool:     return os << "bool";
	case Type::Atom::Kind::Inferred: return os << "(inferred)";
	}

	// we know it's an integer now
	Type::Atom::Integer int_ = atom.get_integer();
	os << (int_.is_signed() ? "int" : "uint");
	Type::Atom::Integer::WidthType width_type = int_.width_type();

	switch (width_type) {
	case Type::Atom::Integer::WidthType::Fixed: return os << int_.bit_width().value();
	case Type::Atom::Integer::WidthType::Any:   return os;
	case Type::Atom::Integer::WidthType::Ptr:   return os << "ptr";
	case Type::Atom::Integer::WidthType::Size:  return os << "size";
	}
	[[assume(false)]];
}

std::ostream& operator<<(std::ostream& os, Type::Pointer const& pointer) {
	return os << "(*" << (pointer.mutable_ ? "mut" : "const") << ' ' << pointer.type->value << ")";
}

std::ostream& operator<<(std::ostream& os, Type const& type) {
	switch (type.kind()) {
	case Type::Kind::Atom:    return os << type.get_atom();
	case Type::Kind::Pointer: return os << type.get_pointer();
	}
	[[assume(false)]];
}

}  // namespace AST
