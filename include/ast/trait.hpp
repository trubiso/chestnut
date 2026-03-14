#pragma once
#include "ast/function.hpp"
#include "ast/identifier.hpp"
#include "ast/type.hpp"

namespace AST {

struct Trait {
	/// Points to a named trait.
	struct Constraint {
		Spanned<Identifier> name;
	};

	Spanned<Name> name;

	std::optional<GenericDeclaration> generic_declaration;

	std::vector<Constraint> constraints;

	std::vector<Function> methods = {};
};

std::ostream& operator<<(std::ostream&, Trait::Constraint const&);
std::ostream& operator<<(std::ostream&, Trait const&);

struct TraitImplementation {
	std::optional<GenericDeclaration> generic_declaration;

	Spanned<Type>              type;  // name of implementer
	Spanned<Type::Atom::Named> name;  // name of implemented trait (we use a named type atom to have a generic list)
	std::vector<Function>      methods;  // implemented methods
};

std::ostream& operator<<(std::ostream&, TraitImplementation const&);

}  // namespace AST
