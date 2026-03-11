#include "resolver.hpp"

#include <algorithm>
#include <cstdlib>

Resolver::TypeInfo::Generic::TraitConstraint Resolver::get_built_in_trait(std::string&& name) const {
	assert(built_in_traits_.contains(name));
	return {built_in_traits_.at(name).name.value.id.value().at(0), {}};
}

std::vector<Resolver::TypeInfo::Generic::TraitConstraint> Resolver::get_implemented_traits(TypeInfo const& type) const {
	if (type.is_same_as()) {
		if (type.get_same_as().ids.size() != 1) return {};
		return get_implemented_traits(type_pool_.at(type.get_same_as().ids.at(0)));
	}
	if (type.is_partial_float() || type.is_known_float()) return {get_built_in_trait("float")};
	if (type.is_partial_integer()) {
		auto const& partial = type.get_partial_integer();
		if (partial.signed_is_known) return {get_built_in_trait(partial.integer.is_signed() ? "sint" : "uint")};
		// FIXME: this implies partial integers implement sint + uint, which is not true, they implement either
		// sint or uint. we have no mechanism to fix this as of right now, though
		return {get_built_in_trait("sint"), get_built_in_trait("uint")};
	}
	if (type.is_known_integer())
		return {get_built_in_trait(type.get_known_integer().integer.is_signed() ? "sint" : "uint")};

	return {};
}

std::optional<bool> Resolver::satisfies_trait_constraint(
	TypeInfo::ID                                           type_id,
	std::vector<TypeInfo::Generic::TraitConstraint> const& constraints,
	std::unordered_map<TypeInfo::ID, TypeInfo::ID> const&  generic_map
) const {
	if (type_pool_.at(type_id).is_generic()) {
		return satisfies_trait_constraint(
			type_pool_.at(type_id).get_generic().declared_constraints,
			constraints,
			generic_map
		);
	} else if (type_pool_.at(type_id).is_member_access() || type_pool_.at(type_id).is_unknown()) {
		// we delay generic resolution if we don't yet know the type
		return std::nullopt;
	} else if (type_pool_.at(type_id).is_bottom()) {
		// we ignore bottoms
		return true;
	} else if (type_pool_.at(type_id).is_same_as()) {
		if (type_pool_.at(type_id).get_same_as().ids.size() != 1) return false;
		return satisfies_trait_constraint(
			type_pool_.at(type_id).get_same_as().ids.at(0),
			constraints,
			generic_map
		);
	} else {
		return satisfies_trait_constraint(
			get_implemented_traits(type_pool_.at(type_id)),
			constraints,
			generic_map
		);
	}
}

Resolver::TypeInfo::Generic::TraitConstraint Resolver::instantiate_constraint(
	TypeInfo::Generic::TraitConstraint const&             constraint,
	std::unordered_map<TypeInfo::ID, TypeInfo::ID> const& generic_map
) const {
	// if everything has been done properly, the generic map should not be mutated and no new types should be
	// created, so a couple const casts here won't hurt (please don't kill me)
	std::vector<TypeInfo::ID> arguments = const_cast<Resolver*>(this)->instantiate_types(
		constraint.arguments,
		const_cast<std::unordered_map<TypeInfo::ID, TypeInfo::ID>&>(generic_map)
	);
	return TypeInfo::Generic::TraitConstraint {constraint.name, std::move(arguments)};
}

std::vector<Resolver::TypeInfo::Generic::TraitConstraint>
Resolver::get_all_constraints(TypeInfo::Generic const& generic) const {
	std::vector<TypeInfo::Generic::TraitConstraint> generic_constraints {};
	std::copy(
		generic.declared_constraints.cbegin(),
		generic.declared_constraints.cend(),
		std::back_inserter(generic_constraints)
	);
	for (auto const& constraint : generic.imposed_constraints) {
		if (std::holds_alternative<TypeInfo::Generic::TraitConstraint>(constraint))
			generic_constraints.push_back(std::get<TypeInfo::Generic::TraitConstraint>(constraint));
		else {
			auto constraints = get_implemented_traits(
				type_pool_.at(std::get<TypeInfo::Generic::TypeConstraint>(constraint).type)
			);
			std::move(constraints.begin(), constraints.end(), std::back_inserter(generic_constraints));
		}
	}

	return generic_constraints;
}

std::vector<Resolver::TypeInfo::Generic::TraitConstraint>
Resolver::expand_trait(TypeInfo::Generic::TraitConstraint const& trait_constraint) const {
	auto const& subconstraints = symbol_pool_.at(trait_constraint.name).trait_constraints;
	auto const& trait          = *std::get<AST::Trait*>(symbol_pool_.at(trait_constraint.name).item);
	// we create a generic map by comparing our generics and the trait's
	std::unordered_map<TypeInfo::ID, TypeInfo::ID> generic_map {};
	if (trait.generic_declaration.has_value()) {
		assert(trait_constraint.arguments.size() == trait.generic_declaration.value().generics.size());
		for (size_t i = 0; i < trait.generic_declaration.value().generics.size(); ++i) {
			auto const& generic = trait.generic_declaration.value().generics.at(i);
			generic_map.insert_or_assign(
				symbol_pool_.at(generic.name.value.id.value().at(0)).type,
				trait_constraint.arguments.at(i)
			);
		}
	}
	std::vector<Resolver::TypeInfo::Generic::TraitConstraint> expanded {trait_constraint};
	for (auto const& subconstraint : subconstraints) {
		// we must instantiate every subconstraint to use this constraint's generic arguments!
		auto actual_subconstraint = instantiate_constraint(subconstraint, generic_map);
		auto subexpansion         = expand_trait(actual_subconstraint);
		std::move(subexpansion.begin(), subexpansion.end(), std::back_inserter(expanded));
	}
	return expanded;
}

std::optional<std::vector<Resolver::TypeInfo::Generic::TraitConstraint>>
Resolver::reduce_to_unique(std::vector<TypeInfo::Generic::TraitConstraint>&& traits) const {
	std::vector<TypeInfo::Generic::TraitConstraint> constraints {};
	for (auto& constraint : traits) {
		bool found = false;
		for (auto const& given_constraint : constraints) {
			auto equality
				= check_bound_equality(constraint, given_constraint, BoundEqualityMode::BoundEqual);
			if (!equality.has_value()) return std::nullopt;
			if (equality.value()) {
				found = true;
				break;
			}
		}
		if (!found) constraints.push_back(std::move(constraint));
	}
	return constraints;
}

std::optional<bool> Resolver::satisfies_trait_constraint(
	std::vector<TypeInfo::Generic::TraitConstraint> const& checked,
	std::vector<TypeInfo::Generic::TraitConstraint> const& other,
	std::unordered_map<TypeInfo::ID, TypeInfo::ID> const&  generic_map
) const {
	std::vector<TypeInfo::Generic::TraitConstraint> expanded_traits {};

	for (auto const& trait_constraint : checked) {
		std::vector<TypeInfo::Generic::TraitConstraint> expanded = expand_trait(trait_constraint);
		std::move(expanded.begin(), expanded.end(), std::back_inserter(expanded_traits));
	}

	auto maybe_expanded = reduce_to_unique(std::move(expanded_traits));
	if (!maybe_expanded.has_value()) return std::nullopt;
	expanded_traits = std::move(maybe_expanded.value());

	for (auto const& trait_constraint : other) {
		auto instantiated_constraint = instantiate_constraint(trait_constraint, generic_map);
		// we need to check that this trait constraint exists in the expanded traits list
		bool any_matched = false;
		for (auto const& trait : expanded_traits) {
			auto equality
				= check_bound_equality(trait, instantiated_constraint, BoundEqualityMode::AStricter);
			if (!equality.has_value()) return std::nullopt;
			if (!equality.value()) continue;
			any_matched = true;
			break;
		}
		if (!any_matched) return false;
	}
	return true;
}

void Resolver::ensure_has_constraints(AST::GenericDeclaration::Generic& generic, FileContext::ID file_id) {
	auto& generic_type = type_pool_.at(get_single_symbol(generic.name.value).type).get_generic();
	if (!generic_type.declared_constraints.empty()) return;
	if (generic.constraints.empty()) return;
	for (auto& constraint : generic.constraints) {
		auto trait_constraint = generate_constraint(constraint, file_id);
		if (!trait_constraint.has_value()) continue;
		generic_type.declared_constraints.push_back(std::move(trait_constraint.value()));
	}
}

std::optional<Resolver::TypeInfo::Generic::TraitConstraint>
Resolver::generate_constraint(AST::GenericDeclaration::Generic::Constraint& constraint, FileContext::ID file_id) {
	if (!constraint.name.value.id.has_value()
	    || constraint.name.value.id.value().size() != 1
	    || !std::holds_alternative<AST::Trait*>(get_single_symbol(constraint.name.value).item)) {
		// name resolver will already have thrown errors about this
		return std::nullopt;
	}

	AST::Trait& trait = *std::get<AST::Trait*>(get_single_symbol(constraint.name.value).item);

	if (!constraint.generic_list.has_value()
	    || (constraint.generic_list.value().ordered.empty() && constraint.generic_list.value().labeled.empty())) {
		// if we don't have any generics, the trait must have no generics
		if (trait.generic_declaration.has_value() && !trait.generic_declaration.value().generics.empty())
			// TODO: diagnostic
			return std::nullopt;

		return TypeInfo::Generic::TraitConstraint {constraint.name.value.id.value()[0], {}};
	}

	// if we do have generics, the trait must have the same quantity
	// TODO: diagnostic
	if (!trait.generic_declaration.has_value()) return std::nullopt;
	auto& trait_generics = trait.generic_declaration.value().generics;
	// TODO: default generics (default everything to inferred by default)
	size_t generic_count
		= constraint.generic_list.value().ordered.size() + constraint.generic_list.value().labeled.size();
	// TODO: diagnostic
	if (generic_count != trait_generics.size()) return std::nullopt;

	// check that all labeled generics exist, ignoring ordered ones
	std::vector<std::string_view> under_consideration {};
	under_consideration.reserve(trait_generics.size() - constraint.generic_list.value().ordered.size());
	for (size_t i = constraint.generic_list.value().ordered.size(); i < trait_generics.size(); ++i)
		if (!trait_generics.at(i).anonymous)
			under_consideration.push_back(trait_generics.at(i).name.value.name());

	if (std::any_of(
		    constraint.generic_list.value().labeled.cbegin(),
		    constraint.generic_list.value().labeled.cend(),
		    [&under_consideration](auto const& generic) {
			    auto const& name = std::get<0>(generic);
			    return !std::any_of(
				    under_consideration.cbegin(),
				    under_consideration.cend(),
				    [&name](std::string_view actual) { return name.value == actual; }
			    );
		    }
	    ))
		// TODO: diagnostic
		return std::nullopt;

	// this is a match! reconstruction time
	std::vector<TypeInfo::ID> arguments {};
	arguments.reserve(generic_count);
	std::transform(
		constraint.generic_list.value().ordered.begin(),
		constraint.generic_list.value().ordered.end(),
		std::back_inserter(arguments),
		[this, file_id](auto& type) {
			return register_type(from_type(type.value, file_id, false), type.span, file_id);
		}
	);
	for (size_t i = arguments.size(); i < trait_generics.size(); ++i) {
		auto corresponding_generic = std::find_if(
			constraint.generic_list.value().labeled.begin(),
			constraint.generic_list.value().labeled.end(),
			[&trait_generics, i](auto& generic) {
				return std::get<0>(generic).value == trait_generics.at(i).name.value.name();
			}
		);
		auto& type = std::get<1>(*corresponding_generic);
		arguments.push_back(register_type(from_type(type.value, file_id, false), type.span, file_id));
	}

	// add trait constraints from the trait generics
	assert(arguments.size() == trait_generics.size());
	for (size_t i = 0; i < arguments.size(); ++i) {
		ensure_has_constraints(trait_generics.at(i), get_single_symbol(constraint.name.value).file_id);
		// FIXME: instantiate entire trait
		std::unordered_map<TypeInfo::ID, TypeInfo::ID> generic_map;
		auto generic = instantiate_type(get_single_symbol(trait_generics.at(i).name.value).type, generic_map);
		unify(arguments.at(i), generic, file_id);
	}

	return TypeInfo::Generic::TraitConstraint {constraint.name.value.id.value()[0], std::move(arguments)};
}
