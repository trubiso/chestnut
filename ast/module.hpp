#pragma once
#include "../span.hpp"
#include "function.hpp"
#include "identifier.hpp"
#include "qualified_identifier.hpp"
#include "tag.hpp"

#include <variant>
#include <vector>

namespace AST {

// TODO: more complex imports
struct Import {
	Spanned<QualifiedIdentifier> name;
};

// TODO: add an item for constants
struct Module {
	Spanned<Identifier> name;

	// we cannot make this a struct, because C++ does not allow incomplete types in variants (which is fair, but we
	// know this will be on the heap anyways).
	// the vector of tags stores all tags that modify this module item.
	// the boolean is whether this item is exported or not.
	typedef std::variant<Function, Module, Import> InnerItem;
	using Item = std::tuple<std::vector<Tag>, bool, InnerItem>;

	struct Body {
		std::vector<Spanned<Item>> items;
	} body;
};

std::ostream& operator<<(std::ostream&, Import const&);
std::ostream& operator<<(std::ostream&, Module::InnerItem const&);
std::ostream& operator<<(std::ostream&, Module::Item const&);
std::ostream& operator<<(std::ostream&, Module const&);

}  // namespace AST
