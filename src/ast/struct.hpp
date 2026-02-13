#pragma once
#include "identifier.hpp"
#include "type.hpp"

namespace AST {

struct Struct {
	Spanned<Identifier> name;

	struct Field {
		// TODO: public/private
		// FIXME: this should be a string, since this will never get resolved :P
		Spanned<Identifier> name;  // unqualified
		Spanned<Type>       type;
	};

	std::vector<Field> fields;
};

std::ostream& operator<<(std::ostream&, Struct const&);

}  // namespace AST
