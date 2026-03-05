#pragma once
#include "ast/identifier.hpp"
#include "ast/type.hpp"

namespace AST {

struct Struct {
	Spanned<Identifier> name;

	std::optional<GenericDeclaration> generic_declaration;

	struct Field {
		// TODO: public/private
		Spanned<std::string> name;
		Spanned<Type>        type;

		std::optional<uint32_t> type_id;
	};

	std::vector<Field> fields;
};

std::ostream& operator<<(std::ostream&, Struct const&);

}  // namespace AST
