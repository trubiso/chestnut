#include "identifier.hpp"

namespace AST {

std::ostream& operator<<(std::ostream& os, Identifier const& identifier) {
	if (identifier.absolute) os << "::";
	for (size_t i = 0; i < identifier.path.size(); ++i) {
		os << identifier.path[i].value;
		if (i + 1 < identifier.path.size()) os << "::";
	}
	if (identifier.id.has_value()) { os << " (@" << identifier.id.value() << ')'; }
	return os;
}

}  // namespace AST
