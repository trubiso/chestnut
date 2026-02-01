#pragma once
#include "../span.hpp"
#include "identifier.hpp"
#include "statement.hpp"
#include "type.hpp"

#include <vector>

namespace AST {

struct Function {
	struct Argument {
		Spanned<Identifier> name;  // unqualified
		Spanned<Type>       type;
		bool                anonymous;
	};

	Spanned<Identifier>   name;  // unqualified
	std::vector<Argument> arguments;
	Spanned<Type>         return_type;

	std::optional<Scope> body;
};

std::ostream& operator<<(std::ostream&, Function const&);

}  // namespace AST
