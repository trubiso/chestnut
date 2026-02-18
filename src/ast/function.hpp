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
		bool                mutable_;
	};

	Spanned<Identifier>               name;  // unqualified
	std::optional<GenericDeclaration> generic_declaration;
	std::vector<Argument>             arguments;
	Spanned<Type>                     return_type;

	std::optional<Scope> body;

	Statement::Label::ID label_counter = 1;
};

std::ostream& operator<<(std::ostream&, Function const&);

}  // namespace AST
