#pragma once
#include "../span.hpp"

#include <iostream>
#include <vector>

namespace AST {

struct Identifier {
	/// Whether the identifier is an absolutely qualified identifier or not.
	bool absolute;
	/// The candidates for this identifier's ID. It starts out as std::nullopt, but becomes a vector of candidates
	/// which can have 0 elements, meaning no symbol matches this identifier.
	// TODO: maybe swap out for a set
	std::optional<std::vector<uint32_t>> id;
	// FIXME: this should absolutely be a SmallVec to avoid heap allocs. also, we're storing the span twice if we do
	// Spanned<Identifier> and it's unqualified.
	/// The path for the qualified identifier. For unqualified identifiers, this holds a single value.
	std::vector<Spanned<std::string>> path;

	/// Whether the identifier refers to an unqualified identifier.
	inline bool is_unqualified() const { return !absolute && path.size() == 1; }

	/// The unqualified identifier's name (can only be called if is_unqualified()).
	std::string const& name() const {
		assert(is_unqualified());
		return path[0].value;
	}

	Spanned<Identifier> extract_unqualified_with_span() const {
		assert(is_unqualified());
		return Spanned<Identifier> {path[0].span, *this};
	}

	inline Spanned<std::string> const& last_fragment() const { return path.at(path.size() - 1); }

	/// Constructor for unqualified identifiers.
	explicit Identifier(Spanned<std::string>&& value) : absolute {false}, id {}, path {std::move(value)} {}

	/// Constructor for qualified identifiers.
	explicit Identifier(bool absolute, std::vector<Spanned<std::string>>&& path)
		: absolute {absolute}
		, id {}
		, path {path} {
		assert(!path.empty());  // empty paths for identifiers make no sense
	}
};

std::ostream& operator<<(std::ostream&, Identifier const&);

}  // namespace AST
