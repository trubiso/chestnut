#pragma once
#include <string>

namespace AST {

struct Tag {
	std::string identifier;
};

std::ostream& operator<<(std::ostream&, Tag const&);

};
