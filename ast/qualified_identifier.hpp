#pragma once
#include "../span.hpp"
#include "identifier.hpp"

#include <iostream>
#include <string_view>
#include <vector>

namespace AST {

// this is a superset of unqualified identifiers, as .absolute = false, .path = [<id>] is an unqualified identifier
struct QualifiedIdentifier {
	bool                    absolute;
	std::optional<uint32_t> id;
	// FIXME: this should absolutely be a SmallVec to avoid heap allocs
	std::vector<Spanned<std::string_view>> path;

	inline bool is_unqualified() const { return !absolute && path.size() == 1; }

	inline Spanned<Identifier> get_unqualified() const {
		return Spanned<Identifier> {
			path[0].span,
			Identifier {path[0].value, id}
		};
	}
};

std::ostream& operator<<(std::ostream&, QualifiedIdentifier const&);

}  // namespace AST
