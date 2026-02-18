#pragma once
#include "../span.hpp"
#include "type.hpp"

#include <string>
#include <vector>

namespace AST {

struct GenericDeclaration {
	struct Generic {
		Spanned<Identifier> name;  // unqualified
		// TODO: trait constraints
		bool anonymous;
	};

	std::vector<Generic> generics;
};

std::ostream& operator<<(std::ostream&, GenericDeclaration const&);

struct GenericList {
	typedef Spanned<Type>                                    OrderedGeneric;
	typedef std::tuple<Spanned<std::string>, OrderedGeneric> LabeledGeneric;

	typedef std::variant<OrderedGeneric, LabeledGeneric> Generic;

	std::vector<OrderedGeneric> ordered;
	std::vector<LabeledGeneric> labeled;
};

std::ostream& operator<<(std::ostream&, GenericList const&);

}  // namespace AST
