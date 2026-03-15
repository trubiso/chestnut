#include "ast/identifier.hpp"

#include "ast/type.hpp"

namespace AST {

std::ostream& operator<<(std::ostream& os, Name const& name) {
	os << name.name;
	if (name.id.has_value()) os << " (@" << name.id.value() << ')';
	return os;
}

bool Identifier::Segment::is_unreached() const {
	return !candidates.has_value();
}

bool Identifier::Segment::is_error() const {
	return candidates.has_value() && candidates.value().empty();
}

bool Identifier::Segment::is_decided() const {
	return candidates.has_value() && candidates.value().size() == 1;
}

bool Identifier::Segment::is_undecided() const {
	return candidates.has_value() && candidates.value().size() > 1;
}

bool Identifier::Segment::is_plain() const {
	return !generic_list.has_value()
	    || (generic_list.value()->labeled.empty() && generic_list.value()->ordered.empty());
}

SymbolID Identifier::Segment::id() const {
	assert(is_decided() && "attempted to get ID of non-decided segment");
	return candidates.value().at(0);
}

Identifier::Segment::Segment(std::string name, Span span, std::optional<std::unique_ptr<GenericList>> generic_list)
	: name {std::move(name)}
	, generic_list {std::move(generic_list)}
	, candidates {std::nullopt}
	, span {std::move(span)} {}

Identifier::Segment::Segment(std::string name, Span span) : Segment {std::move(name), std::move(span), std::nullopt} {}

Spanned<std::string> Identifier::Segment::spanned_name() const {
	return {span, name};
}

bool Identifier::is_decided() const {
	return std::all_of(path_.cbegin(), path_.cend(), [](Segment const& segment) { return segment.is_decided(); });
}

SymbolID Identifier::id() const {
	assert(is_decided() && "attempted to get ID of non-decided identifier");
	return path_.back().id();
}

bool Identifier::has_middle_undecided() const {
	return std::any_of(path_.cbegin(), path_.cbegin() + (path_.size() - 1), [](Segment const& segment) {
		return segment.is_undecided();
	});
}

bool Identifier::has_final_undecided() const {
	return path_.back().is_undecided();
}

bool Identifier::has_at_least_one_id() const {
	// if has_final_undecided(), the rest must be decided, otherwise name resolution wouldn't have gotten this far.
	return is_decided() || has_final_undecided();
}

std::vector<SymbolID> const& Identifier::ids() const {
	assert(has_at_least_one_id());
	return path_.back().candidates.value();
}

bool Identifier::has_undecided() const {
	return std::any_of(path_.cbegin(), path_.cend(), [](Segment const& segment) { return segment.is_undecided(); });
}

bool Identifier::is_error() const {
	return std::any_of(path_.cbegin(), path_.cend(), [](Segment const& segment) { return segment.is_error(); });
}

bool Identifier::is_plain() const {
	return std::all_of(path_.cbegin(), path_.cend(), [](Segment const& segment) { return segment.is_plain(); });
}

bool Identifier::is_name() const {
	return path_.size() == 1 && is_plain();
}

std::string const& Identifier::get_name() const {
	assert(is_name() && "attempted to get name of non-name identifier");
	return path_.back().name;
}

Identifier::Segment& Identifier::last_undecided_segment() {
	assert((has_undecided()) && "attempted to get last undecided segment of non-undecided identifier");
	for (size_t i = path_.size(); i > 0; --i) {
		if (path_.at(i - 1).is_undecided()) return path_.at(i - 1);
	}
	assert(false);
}

bool Identifier::has_unreached() const {
	return std::any_of(path_.cbegin(), path_.cend(), [](Segment const& segment) { return segment.is_unreached(); });
}

Identifier::Segment& Identifier::first_unreached_segment() {
	assert((has_unreached()) && "attempted to get first unreached segment of non-unreached identifier");
	for (size_t i = 0; i < path_.size(); ++i) {
		if (path_.at(i).is_unreached()) return path_.at(i);
	}
	assert(false);
}

Identifier::Segment& Identifier::root() {
	return path_.at(0);
}

bool Identifier::can_be_name_resolved() const {
	return !is_error() && !has_undecided() && has_unreached();
}

Identifier::Segment& Identifier::last_decided_segment_before_unreached() {
	assert(can_be_name_resolved());
	assert(!is_decided());  // is_decided() => !has_unreached() => !can_be_name_resolved().
	for (size_t i = path_.size(); i > 0; --i) {
		if (path_.at(i - 1).is_decided()) {
			// the last decided segment must be immediately before the unreached segment.
			assert(path_.at(i).is_unreached());  // safety: i == path_.size() => is_decided().
			return path_.at(i - 1);
		}
	}
	assert(false && "attempted to get last decided segment, but no such segments exist!");
}

bool Identifier::last_is_only_unreached() const {
	return std::all_of(
		       path_.cbegin(),
		       path_.cbegin() + (path_.size() - 1),
		       [](Segment const& segment) { return segment.is_decided(); }
	       )
	    && path_.back().is_unreached();
}

Identifier::Identifier(Spanned<std::string> name) : absolute_ {false}, path_ {} {
	path_.emplace_back(std::move(name.value), std::move(name.span), std::nullopt);
}

Identifier::Identifier(bool absolute, std::vector<Segment> path) : absolute_ {absolute}, path_ {std::move(path)} {
	// TODO: validate path more thoroughly
	assert(!path_.empty() && "attempted to create identifier with empty path");
}

Identifier::Identifier(Span span, Name name) : absolute_ {false}, path_ {} {
	Segment segment {std::move(name.name), std::move(span), std::nullopt};
	if (name.id.has_value()) segment.candidates = {name.id.value()};
	path_.push_back(std::move(segment));
}

std::ostream& operator<<(std::ostream& os, Identifier::Segment const& segment) {
	os << segment.name;
	if (segment.generic_list.has_value()) os << *segment.generic_list.value();
	if (segment.generic_bindings.has_value()) {
		if (segment.generic_bindings.value().empty()) {
			os << " (bound)";
		} else {
			os << " (bindings: ";
			size_t count = 0;
			for (auto [a, b] : segment.generic_bindings.value()) {
				os << '@' << a << " -> $" << b;
				if (++count < segment.generic_bindings.value().size()) os << ", ";
			}
			os << ')';
		}
	}
	if (segment.is_unreached()) return os;
	if (segment.is_error()) return os << " (error)";
	os << " (";
	size_t count = 0;
	for (SymbolID id : segment.candidates.value()) {
		os << '@' << id;
		if (++count < segment.candidates.value().size()) os << ", ";
	}
	return os << ')';
}

std::ostream& operator<<(std::ostream& os, Identifier const& identifier) {
	if (identifier.absolute()) os << "::";
	for (size_t i = 0; i < identifier.path().size(); ++i) {
		os << identifier.path().at(i);
		if (i + 1 < identifier.path().size()) os << "::";
	}
	return os;
}

}  // namespace AST
