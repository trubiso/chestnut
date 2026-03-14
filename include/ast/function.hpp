#pragma once
#include "ast/identifier.hpp"
#include "ast/statement.hpp"
#include "ast/type.hpp"
#include "span.hpp"

#include <vector>

namespace AST {

struct Function {
	struct Argument {
		Spanned<OldIdentifier> name;  // unqualified
		Spanned<Type>       type;
		bool                anonymous;
		bool                mutable_;
	};

	Spanned<OldIdentifier>               name;  // unqualified
	std::optional<GenericDeclaration> generic_declaration;
	std::vector<Argument>             arguments;
	Spanned<Type>                     return_type;

	std::optional<Scope> body;

	Statement::Label::ID label_counter = 1;
};

std::ostream& operator<<(std::ostream&, Function const&);

}  // namespace AST
