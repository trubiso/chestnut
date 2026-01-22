#include "type.hpp"

namespace AST {

std::ostream& operator<<(std::ostream& os, Type const& type) {
	switch (type.kind()) {
	case Type::Kind::Integer: break;
	case Type::Kind::Float:   return os << "float" << (uint32_t) type.get_float().width_value();
	case Type::Kind::Void:    return os << "void";
	case Type::Kind::Char:    return os << "char";
	case Type::Kind::Bool:    return os << "bool";
	}

	// we know it's an integer now
	Type::Integer int_ = type.get_integer();
	os << (int_.is_signed() ? "int" : "uint");
	Type::Integer::WidthType width_type = int_.width_type();

	switch (width_type) {
	case Type::Integer::WidthType::Fixed: return os << int_.bit_width().value();
	case Type::Integer::WidthType::Any:   return os;
	case Type::Integer::WidthType::Ptr:   return os << "ptr";
	case Type::Integer::WidthType::Size:  return os << "size";
	}
}

}  // namespace AST
