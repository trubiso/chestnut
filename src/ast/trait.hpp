#pragma once
#include "identifier.hpp"
#include "type.hpp"

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
};

std::ostream& operator<<(std::ostream&, Trait::Constraint const&);
std::ostream& operator<<(std::ostream&, Trait const&);

}  // namespace AST
