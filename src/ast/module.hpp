#pragma once
#include "../span.hpp"
#include "function.hpp"
#include "identifier.hpp"
#include "struct.hpp"
#include "tag.hpp"

#include <variant>
#include <vector>

namespace AST {

struct Alias {
	Spanned<Identifier> name;  // unqualified
	Spanned<Identifier> value;
};

/// Imports mark items external to a module as available from within them.
struct Import {
	Spanned<Identifier> name;
};

// TODO: add an item for constants
struct Module {
	Spanned<Identifier> name;  // unqualified

	// we cannot make this a struct, because C++ does not allow incomplete types in variants (which is fair, but we
	// know this will be on the heap anyways).
	// the vector of tags stores all tags that modify this module item.
	// the boolean is whether this item is exported or not.
	typedef std::variant<Function, Module, Alias, Import, Struct> InnerItem;

	static std::string const& get_name(InnerItem const& inner_item);

	using Item = std::tuple<std::vector<Tag>, bool, InnerItem>;

	static inline std::string const& get_name(Item const& item) { return get_name(std::get<InnerItem>(item)); }

	struct Body {
		std::vector<Spanned<Item>> items;
	} body;
};

std::ostream& operator<<(std::ostream&, Alias const&);
std::ostream& operator<<(std::ostream&, Import const&);
std::ostream& operator<<(std::ostream&, Module::InnerItem const&);
std::ostream& operator<<(std::ostream&, Module::Item const&);
std::ostream& operator<<(std::ostream&, Module const&);

}  // namespace AST
