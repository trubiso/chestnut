#include "identifier.hpp"
#include "type.hpp"

namespace AST {

struct Trait {
	/// Points to a named trait.
	struct Named {
		Spanned<Identifier> name;
	};

	/// Concrete constraint.
	struct Has {
		// TODO: support functions returning functions

		Spanned<Identifier> name;

		struct Argument {
			Spanned<Identifier> name;  // unqualified
			Spanned<Type>       type;
			bool                anonymous;
		};

		std::optional<GenericDeclaration> generic_declaration;
		std::vector<Argument>             arguments;
		Spanned<Type>                     return_type;
	};

	typedef std::variant<Named, Has> Constraint;

	Spanned<Identifier> name;

	std::optional<GenericDeclaration> generic_declaration;

	std::vector<Constraint> constraints;
};

std::ostream& operator<<(std::ostream&, Trait::Named const&);
std::ostream& operator<<(std::ostream&, Trait::Has const&);
std::ostream& operator<<(std::ostream&, Trait::Constraint const&);
std::ostream& operator<<(std::ostream&, Trait const&);

}  // namespace AST
