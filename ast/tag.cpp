#include "tag.hpp"
#include <iostream>

namespace AST {

std::ostream& operator<<(std::ostream& os, Tag const& tag) {
	return os << '@' << tag.identifier;
}

}  // namespace AST
