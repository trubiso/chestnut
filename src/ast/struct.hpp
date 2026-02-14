#pragma once
#include "identifier.hpp"
#include "type.hpp"

namespace AST {

struct Struct {
	Spanned<Identifier> name;

	struct Field {
		// TODO: public/private
		Spanned<std::string> name;
		Spanned<Type>        type;
	};

	std::vector<Field> fields;
};

std::ostream& operator<<(std::ostream&, Struct const&);

}  // namespace AST
