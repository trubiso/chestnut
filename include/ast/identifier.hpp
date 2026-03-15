#pragma once
#include "span.hpp"

#include <iostream>
#include <memory>
#include <vector>

namespace AST {

typedef uint32_t SymbolID;

/// Represents the name of an item in a declaration. This differs from a qualified identifier because there are no
/// associated generics and there is always a single ID.
struct Name {
	/// The name of the item.
	std::string name;
	/// The ID of the item, or null if it hasn't yet been reached by the symbol resolver/identifier.
	std::optional<SymbolID> id;

	explicit Name(std::string name) : name(std::move(name)) {}
};

std::ostream& operator<<(std::ostream&, Name const&);

struct GenericList;

/// Represents a qualified identifier.
struct Identifier {
	/// Represents a segment within an identifier.
	struct Segment {
		/// The name of the segment, that is, what the user typed.
		std::string name;
		/// An optional amount of generics supplied to this name.
		std::optional<std::unique_ptr<GenericList>> generic_list;
		/// Which item(s) this segment could refer to, or null if it hasn't yet been reached by the symbol
		/// resolver.
		std::optional<std::vector<SymbolID>> candidates;
		/// We embed the span directly into the segment for convenience.
		Span span;

		/// Returns whether candidates is null, i.e. this segment has not yet been reached by the symbol
		/// resolver.
		bool is_unreached() const;
		/// Returns whether candidates is empty, i.e. there are no suitable candidates.
		bool is_error() const;
		/// Returns whether candidates has a single value.
		bool is_decided() const;
		/// Returns whether candidates has several values, i.e. there are several candidates.
		bool is_undecided() const;
		/// Returns whether the generic list is null or empty.
		bool is_plain() const;
		/// Returns the segment's ID, assuming is_decided().
		SymbolID id() const;

		explicit Segment(std::string name, Span span, std::optional<std::unique_ptr<GenericList>> generic_list);
		explicit Segment(std::string name, Span span);

		/// Returns the segment as its spanned name.
		Spanned<std::string> spanned_name() const;
	};

	/// Returns whether all segments are decided.
	bool is_decided() const;
	/// Returns the identifier's ID (its last segment's single ID), assuming is_decided().
	SymbolID id() const;
	/// Returns whether a non-final segment is undecided.
	bool has_middle_undecided() const;
	/// Returns whether the final segment is undecided.
	bool has_final_undecided() const;
	/// Returns whether all non-final segments are decided and the last segment is either decided or undecided.
	bool has_at_least_one_id() const;
	/// Returns the identifier's IDs (its last segment's IDs), assuming has_at_least_one_id().
	std::vector<SymbolID> const& ids() const;
	/// Returns whether any segment is undecided.
	bool has_undecided() const;
	/// Returns whether any segment is an error.
	bool is_error() const;
	/// Returns whether there are no generics in the identifier.
	bool is_plain() const;
	/// Returns whether the identifier is plain and single-segment (that is, a name).
	bool is_name() const;
	/// Returns the name of the identifier, assuming is_name().
	std::string const& get_name() const;
	/// Returns the last undecided segment, assuming has_undecided().
	Segment& last_undecided_segment();
	/// Returns whether a segment within the identifier is unreached.
	bool has_unreached() const;
	/// Returns the first unreached segment, assuming has_unreached().
	Segment& first_unreached_segment();
	/// Returns the root segment of the identifier.
	Segment& root();
	/// Returns whether this identifier can be name resolved, i.e. if it does not have any undecided segments but it
	/// does have unreached segments.
	bool can_be_name_resolved() const;
	/// Returns the last decided segment before an unreached segment, assuming one such segment exists. This
	/// function is meant to be used if can_be_name_resolved().
	Segment& last_decided_segment_before_unreached();
	/// Returns whether every segment is decided, except for the last segment, which is unreached.
	bool last_is_only_unreached() const;

	/// Whether the root segment should be resolved absolutely instead of relatively.
	inline bool absolute() const { return absolute_; }

	/// Forces the identifier to be absolute.
	inline void force_absolute() { absolute_ = true; }

	/// Forces the last segment's IDs to a set of values. This operation can only be carried out if the last segment
	/// already had at least one ID, to ensure segment structure isn't broken.
	inline void force_ids(std::vector<SymbolID> ids) {
		assert(has_at_least_one_id());
		path_.back().candidates = std::move(ids);
	}

	inline std::vector<Segment> const& path() const { return path_; }

	/// Constructor for unqualified identifiers.
	explicit Identifier(Spanned<std::string> name);
	/// Constructor for qualified identifiers.
	explicit Identifier(bool absolute, std::vector<Segment> path);
	/// Name to identifier promotion.
	explicit Identifier(Span, Name);

private:
	bool                 absolute_;
	std::vector<Segment> path_;
};

std::ostream& operator<<(std::ostream&, Identifier::Segment const&);
std::ostream& operator<<(std::ostream&, Identifier const&);

}  // namespace AST
