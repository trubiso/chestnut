#include "identifier.hpp"

namespace AST {

std::ostream& operator<<(std::ostream& os, Identifier const& identifier) {
	os << identifier.name;
	if (identifier.id.has_value()) { os << " (@" << identifier.id.value() << ')'; }
	return os;
}

}  // namespace AST
