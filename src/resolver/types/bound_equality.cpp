#include "resolver.hpp"

#include <algorithm>
#include <cstdlib>

constexpr Resolver::BoundEqualityMode Resolver::invert_bound_equality_mode(BoundEqualityMode mode) {
	if (mode == BoundEqualityMode::BoundEqual) return mode;
	if (mode == BoundEqualityMode::BStricter) return BoundEqualityMode::AStricter;
	return BoundEqualityMode::BStricter;
}

std::optional<bool> Resolver::check_bound_equality_function(TypeInfo const&, TypeInfo const&, BoundEqualityMode) const {
	assert(false && "why are we checking bound equality for functions when function types are not supported?");
}

std::optional<bool>
Resolver::check_bound_equality_same_as(TypeInfo const& same_as, TypeInfo const& other, BoundEqualityMode mode) const {
	if (same_as.get_same_as().ids.size() != 1) return std::nullopt;
	return check_bound_equality(type_pool_.at(same_as.get_same_as().ids.at(0)), other, mode);
}

std::optional<bool>
Resolver::check_bound_equality_generic(TypeInfo const& generic, TypeInfo const& other, BoundEqualityMode mode) const {
	std::vector<TypeInfo::Generic::TraitConstraint> a {}, b {};

	for (auto const& trait_constraint : generic.get_generic().declared_constraints) {
		std::vector<TypeInfo::Generic::TraitConstraint> expanded = expand_trait(trait_constraint);
		std::move(expanded.begin(), expanded.end(), std::back_inserter(a));
	}

	auto maybe_a = reduce_to_unique(std::move(a));
	if (!maybe_a.has_value()) return std::nullopt;
	a = std::move(maybe_a.value());

	// the second set of traits comes from the declared traits of the other generic or the implemented traits
	// otherwise
	if (other.is_generic()) {
		// we need the names to be equal for the generics to be equal
		if (generic.get_generic().name != other.get_generic().name) return false;

		for (auto const& trait_constraint : other.get_generic().declared_constraints) {
			std::vector<TypeInfo::Generic::TraitConstraint> expanded = expand_trait(trait_constraint);
			std::move(expanded.begin(), expanded.end(), std::back_inserter(b));
		}

		auto maybe_b = reduce_to_unique(std::move(b));
		if (!maybe_b.has_value()) return std::nullopt;
		b = std::move(maybe_b.value());
	} else {
		// FIXME: this should expand before reducing to unique!
		auto maybe_b = reduce_to_unique(get_implemented_traits(other));
		if (!maybe_b.has_value()) return std::nullopt;
		b = std::move(maybe_b.value());
	}

	if (mode != BoundEqualityMode::AStricter)
		for (auto const& trait_constraint : a) {
			// we need to check that this trait constraint exists in the other expanded traits list
			bool any_matched = false;
			for (auto const& trait : b) {
				auto equality = check_bound_equality(trait_constraint, trait, mode);
				if (!equality.has_value()) return std::nullopt;
				if (!equality.value()) continue;
				any_matched = true;
				break;
			}
			if (!any_matched) return false;
		}

	if (mode != BoundEqualityMode::BStricter)
		for (auto const& trait_constraint : b) {
			// we need to check that this trait constraint exists in the other expanded traits list
			bool any_matched = false;
			for (auto const& trait : a) {
				auto equality = check_bound_equality(trait_constraint, trait, mode);
				if (!equality.has_value()) return std::nullopt;
				if (!equality.value()) continue;
				any_matched = true;
				break;
			}
			if (!any_matched) return false;
		}

	return true;
}

std::optional<bool>
Resolver::check_bound_equality_named(TypeInfo const& named, TypeInfo const& other, BoundEqualityMode mode) const {
	if (!other.is_named()) return false;
	int decided = named.is_decided(type_pool_);
	if (decided == -1) {
		// if it is impossible, it acts as a bottom
		return true;
	} else if (decided == 0) {
		// if it is to be determined, we must wait
		return std::nullopt;
	}
	decided = other.is_decided(type_pool_);
	if (decided == -1) {
		// if it is impossible, it acts as a bottom
		return true;
	} else if (decided == 0) {
		// if it is to be determined, we must wait
		return std::nullopt;
	}

	auto const &a_candidate = named.get_named().candidates().at(0),
		   &b_candidate = other.get_named().candidates().at(0);

	if (a_candidate.name != b_candidate.name) return false;
	if (a_candidate.generics.size() != b_candidate.generics.size()) return false;

	for (size_t i = 0; i < a_candidate.generics.size(); ++i) {
		auto equality = check_bound_equality(a_candidate.generics.at(i), b_candidate.generics.at(i), mode);
		if (!equality.has_value()) return std::nullopt;
		if (!equality.value()) return false;
	}

	return true;
}

std::optional<bool>
Resolver::check_bound_equality_pointer(TypeInfo const& pointer, TypeInfo const& other, BoundEqualityMode mode) const {
	assert(other.is_pointer());
	return check_bound_equality(pointer.get_pointer().pointee, other.get_pointer().pointee, mode);
}

std::optional<bool> Resolver::check_bound_equality(TypeInfo const& a, TypeInfo const& b, BoundEqualityMode mode) const {
	if (!can_unify(a, b)) return false;

	// we can safely assume a, b are unifiable from here on out
	if (a.is_same_as()) return check_bound_equality_same_as(a, b, mode);
	if (b.is_same_as()) return check_bound_equality_same_as(b, a, invert_bound_equality_mode(mode));
	if (a.is_bottom() || b.is_bottom()) return true;
	// these can only come from unresolved overloads or unconstrained types
	if (a.is_unknown() || b.is_unknown()) return std::nullopt;
	// member accesses get replaced after being resolved, so this is not resolved either
	if (a.is_member_access() || b.is_member_access()) return std::nullopt;

	// generics
	if (a.is_generic()) return check_bound_equality_generic(a, b, mode);
	if (b.is_generic()) return check_bound_equality_generic(b, a, invert_bound_equality_mode(mode));

	// named types
	if (a.is_named()) return check_bound_equality_named(a, b, mode);
	if (b.is_named()) return check_bound_equality_named(b, a, invert_bound_equality_mode(mode));

	// functions
	if (a.is_function()) return check_bound_equality_function(a, b, mode);
	if (b.is_function()) return check_bound_equality_function(b, a, invert_bound_equality_mode(mode));

	// pointers
	if (a.is_pointer()) return check_bound_equality_pointer(a, b, mode);
	if (b.is_pointer()) return check_bound_equality_pointer(b, a, invert_bound_equality_mode(mode));

	return true;
}

std::optional<bool> Resolver::check_bound_equality(TypeInfo::ID a, TypeInfo::ID b, BoundEqualityMode mode) const {
	return check_bound_equality(type_pool_.at(a), type_pool_.at(b), mode);
}

std::optional<bool> Resolver::check_bound_equality(
	TypeInfo::Generic::TraitConstraint const& a,
	TypeInfo::Generic::TraitConstraint const& b,
	BoundEqualityMode                         mode
) const {
	if (a.name != b.name) return false;
	assert(a.arguments.size() == b.arguments.size());

	for (size_t i = 0; i < a.arguments.size(); ++i) {
		auto equality = check_bound_equality(a.arguments.at(i), b.arguments.at(i), mode);
		if (!equality.has_value()) return std::nullopt;
		if (!equality.value()) return false;
	}

	return true;
}
