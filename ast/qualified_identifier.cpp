#include "qualified_identifier.hpp"

namespace AST {

std::ostream& operator<<(std::ostream& os, QualifiedIdentifier const& identifier) {
	if (identifier.absolute) os << "::";
	for (size_t i = 0; i < identifier.path.size(); ++i) {
		os << identifier.path[i].value;
		if (i + 1 < identifier.path.size()) os << "::";
	}
	return os;
}

}  // namespace AST
