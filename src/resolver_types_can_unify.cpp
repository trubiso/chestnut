#include "resolver.hpp"

#include <algorithm>
#include <cstdlib>

bool Resolver::can_unify_follow_references(TypeInfo const& same_as, TypeInfo const& other) const {
	assert(same_as.is_same_as());
	std::vector<TypeInfo::ID> const& ids = same_as.get_same_as().ids;
	return std::any_of(ids.cbegin(), ids.cend(), [this, other](TypeInfo::ID id) { return can_unify(id, other); });
}

std::optional<bool> Resolver::can_unify_basic_known(TypeInfo::Kind kind, TypeInfo const& a, TypeInfo const& b) const {
	bool a_matches = a.kind() == kind, b_matches = b.kind() == kind;
	if (!a_matches && !b_matches) return std::nullopt;  // if neither match, this case is not for us
	if (a_matches && b_matches) return true;            // if both match, this is a freebie
	return false;                                       // if neither match, it won't work
}

bool Resolver::can_unify_functions(TypeInfo const& function, TypeInfo const& other) const {
	// ensure they're both functions
	assert(function.is_function());
	if (!other.is_function()) { return false; }

	TypeInfo::Function const &a_function = function.get_function(), &b_function = other.get_function();

	// ensure they have the same argument count
	if (a_function.arguments.size() != b_function.arguments.size()) { return false; }

	// unify the names and types of arguments
	for (size_t i = 0; i < a_function.arguments.size(); ++i) {
		auto& [a_name, a_type] = a_function.arguments.at(i);
		auto& [b_name, b_type] = b_function.arguments.at(i);
		if (a_name.has_value() && b_name.has_value() && (a_name.value() != b_name.value())) { return false; }
		if (!can_unify(a_type, b_type)) return false;
	}

	// unify the names and types of generics
	for (size_t i = 0; i < a_function.generics.size(); ++i) {
		auto& [a_name, a_type] = a_function.generics.at(i);
		auto& [b_name, b_type] = b_function.generics.at(i);
		if (a_name.has_value() && b_name.has_value() && (a_name.value() != b_name.value())) { return false; }
		// TODO: we most likely don't need to call this, since generics are of type generic
		if (!can_unify(a_type, b_type)) return false;
	}

	// unify the return types
	return can_unify(a_function.return_, b_function.return_);
}

bool Resolver::can_unify_pointers(TypeInfo const& pointer, TypeInfo const& other) const {
	// ensure they're both pointers
	assert(pointer.is_pointer());
	if (!other.is_pointer()) { return false; }

	TypeInfo::Pointer const &a_pointer = pointer.get_pointer(), &b_pointer = other.get_pointer();

	// ensure they point to the same type
	if (!can_unify(a_pointer.pointee, b_pointer.pointee)) return false;

	// ensure they are of the same mutability
	return a_pointer.mutable_ == b_pointer.mutable_;
}

bool Resolver::can_unify_named(TypeInfo const& named, TypeInfo const& other) const {
	// ensure they're both named types
	assert(named.is_named());
	if (!other.is_named()) return false;

	auto const &a = named.get_named(), &b = other.get_named();

	// check if there are any common candidates
	bool any_common_candidate = false;
	for (TypeInfo::Named::Candidate const& candidate : a.candidates()) {
		auto corresponding_candidate = std::find_if(
			b.candidates().cbegin(),
			b.candidates().cend(),
			[&candidate, this](TypeInfo::Named::Candidate const& other_candidate) {
				if (candidate.name != other_candidate.name) return false;
				if (candidate.generics.size() != other_candidate.generics.size()) return false;
				for (size_t i = 0; i < candidate.generics.size(); ++i) {
					if (!can_unify(candidate.generics.at(i), other_candidate.generics.at(i)))
						return false;
				}
				return true;
			}
		);
		if (corresponding_candidate == b.candidates().cend()) continue;
		any_common_candidate = true;
		break;
	}
	return any_common_candidate;
}

bool Resolver::can_unify(TypeInfo::ID a, TypeInfo::ID b) const {
	return can_unify(type_pool_.at(a), type_pool_.at(b));
}

bool Resolver::can_unify(TypeInfo const& a, TypeInfo::ID b) const {
	return can_unify(a, type_pool_.at(b));
}

bool Resolver::can_unify(TypeInfo::ID a, TypeInfo const& b) const {
	return can_unify(type_pool_.at(a), b);
}

bool Resolver::can_unify(TypeInfo const& a, TypeInfo const& b) const {
	// follow references
	if (a.is_same_as()) return can_unify_follow_references(a, b);
	if (b.is_same_as()) return can_unify_follow_references(b, a);

	// bottoms don't participate in unification
	if (a.is_bottom() || b.is_bottom()) return true;

	// make unknowns known
	if (a.is_unknown()) return true;
	if (b.is_unknown()) return true;

	// generics always unify
	if (a.is_generic()) return true;
	if (b.is_generic()) return true;

	// member access types (these always succeed :P)
	if (a.is_member_access()) return true;
	if (b.is_member_access()) return true;

	// named types
	if (a.is_named()) return can_unify_named(a, b);
	if (b.is_named()) return can_unify_named(b, a);

	// if any of them is a basic Known type, the other must be exactly the same
	std::optional<bool> attempt;
	attempt = can_unify_basic_known(TypeInfo::Kind::KnownVoid, a, b);
	if (attempt.has_value()) return attempt.value();
	attempt = can_unify_basic_known(TypeInfo::Kind::KnownChar, a, b);
	if (attempt.has_value()) return attempt.value();
	attempt = can_unify_basic_known(TypeInfo::Kind::KnownBool, a, b);
	if (attempt.has_value()) return attempt.value();

	// modules act like basic Known types right now, but this is silly.
	// TODO: do something better
	attempt = can_unify_basic_known(TypeInfo::Kind::Module, a, b);
	if (attempt.has_value()) return attempt.value();

	// functions
	if (a.is_function()) return can_unify_functions(a, b);
	if (b.is_function()) return can_unify_functions(b, a);

	// pointers
	if (a.is_pointer()) return can_unify_pointers(a, b);
	if (b.is_pointer()) return can_unify_pointers(b, a);

	// now only numeric types are left ([Known/Partial][Integer/Float])
	bool a_known = a.is_known_integer() || a.is_known_float(), b_known = b.is_known_integer() || b.is_known_float();
	bool a_float = a.is_known_float() || a.is_partial_float(), b_float = b.is_known_float() || b.is_partial_float();

	// both must be either floats or integers
	if (a_float != b_float) return false;

	// floats
	if (a_float) {
		if (!a_known && !b_known) return true;
		if (a_known && b_known && a.get_known_float().width != b.get_known_float().width) return false;
		return true;
	}

	// integers may clash due to sign and size
	if (a_known && b_known) {
		// if both are known, we can just check clashes directly
		if (a.get_known_integer().integer.is_signed() != b.get_known_integer().integer.is_signed())
			return false;
		if (a.get_known_integer().integer.width_type() != b.get_known_integer().integer.width_type())
			return false;
		if (a.get_known_integer().integer.bit_width() != b.get_known_integer().integer.bit_width())
			return false;
		return true;
	} else if (a_known != b_known) {
		// if one isn't known, we must check that there are no clashes
		TypeInfo::PartialInteger const& partial = a_known ? b.get_partial_integer() : a.get_partial_integer();
		TypeInfo::KnownInteger const&   known   = a_known ? a.get_known_integer() : b.get_known_integer();

		// if the sign is known, it must not clash
		if (partial.signed_is_known && partial.integer.is_signed() != known.integer.is_signed()) return false;

		// if the size is known, it must not clash
		if (partial.integer.width_type() != AST::Type::Atom::Integer::WidthType::Any) {
			if (partial.integer.width_type() != known.integer.width_type()) return false;
			else if (partial.integer.bit_width() != known.integer.bit_width()) return false;
		}

		// if no clashes were detected, we unify by setting the partial one to the known one
		return true;
	} else {
		// neither are known, so we will construct a unified partial integer for both of them
		TypeInfo::PartialInteger const &a_partial = a.get_partial_integer(),
					       &b_partial = b.get_partial_integer();

		// if the sign is known for both, it must not clash
		if (a_partial.signed_is_known
		    && b_partial.signed_is_known
		    && a_partial.integer.is_signed() != b_partial.integer.is_signed())
			return false;

		// if the width is known for both, it must not clash
		if (a_partial.integer.width_type() != AST::Type::Atom::Integer::WidthType::Any
		    && b_partial.integer.width_type() != AST::Type::Atom::Integer::WidthType::Any) {
			if (a_partial.integer.width_type() != b_partial.integer.width_type()) return false;
			else if (a_partial.integer.bit_width() != b_partial.integer.bit_width()) return false;
		}
		return true;
	}
}
