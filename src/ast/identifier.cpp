#include "ast/identifier.hpp"

#include "ast/type.hpp"

namespace AST {

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

Identifier::Segment& Identifier::last_unreached_segment() {
	assert((has_unreached()) && "attempted to get last unreached segment of non-unreached identifier");
	for (size_t i = path_.size(); i > 0; --i) {
		if (path_.at(i - 1).is_unreached()) return path_.at(i - 1);
	}
	assert(false);
}

Identifier::Identifier(Spanned<std::string> name) : absolute_ {false}, path_ {} {
	path_.emplace_back(std::move(name.value), std::move(name.span), std::nullopt);
}

Identifier::Identifier(bool absolute, std::vector<Segment> path) : absolute_ {absolute}, path_ {std::move(path)} {
	// TODO: validate path more thoroughly
	assert(!path_.empty() && "attempted to create identifier with empty path");
}

std::ostream& operator<<(std::ostream& os, Identifier::Segment const& segment) {
	os << segment.name;
	if (segment.generic_list.has_value()) os << segment.generic_list.value();
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

std::ostream& operator<<(std::ostream& os, OldIdentifier const& identifier) {
	if (identifier.absolute) os << "::";
	for (size_t i = 0; i < identifier.path.size(); ++i) {
		os << identifier.path[i].value;
		if (i + 1 < identifier.path.size()) os << "::";
	}
	if (identifier.id.has_value()) {
		os << " (";
		size_t count = 0;
		for (SymbolID id : identifier.id.value()) {
			os << '@' << id;
			if (++count < identifier.id.value().size()) os << ", ";
		}
		os << ')';
	}
	return os;
}

}  // namespace AST
