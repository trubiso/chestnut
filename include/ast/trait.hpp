#pragma once
#include "ast/function.hpp"
#include "ast/identifier.hpp"
#include "ast/type.hpp"

namespace AST {

struct Trait {
	/// Points to a named trait.
	struct Constraint {
		Spanned<Identifier> name;

		std::optional<GenericList> generic_list;
	};

	Spanned<Identifier> name;

	std::optional<GenericDeclaration> generic_declaration;

	std::vector<Constraint> constraints;

	std::vector<Function> methods = {};
};

std::ostream& operator<<(std::ostream&, Trait::Constraint const&);
std::ostream& operator<<(std::ostream&, Trait const&);

}  // namespace AST
