#pragma once
#include "../span.hpp"

#include <iostream>
#include <string_view>
#include <vector>

namespace AST {

// this is a superset of unqualified identifiers, as .absolute = false, .path = [<id>] is an unqualified identifier
struct QualifiedIdentifier {
	bool absolute;
	// FIXME: this should absolutely be a SmallVec to avoid heap allocs
	std::vector<Spanned<std::string_view>> path;

	inline bool is_unqualified() const { return !absolute && path.size() == 1; }

	inline Spanned<std::string_view> get_unqualified() const { return path[0]; }
};

std::ostream& operator<<(std::ostream&, QualifiedIdentifier const&);

}  // namespace AST
