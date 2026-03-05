#include "resolver.hpp"

#include <algorithm>
#include <cstdlib>

std::unordered_set<size_t> Resolver::specialize_overload(std::vector<std::vector<size_t>>&& trait_counts) const {
	// TODO: this assumes all overloads have the same generic count, which will break once we have default values.

	// first, we create a set of global compliant overloads
	std::unordered_set<size_t> global_compliant {};
	for (size_t i = 0; i < trait_counts.size(); ++i) global_compliant.insert(i);

	// then, we get the overloads with most specific generics on a per-generic basis
	for (size_t i = 0; i < trait_counts.at(0).size(); ++i) {
		size_t max = trait_counts.at(0).at(i);

		// contains the overloads which are as specific as possible
		std::unordered_set<size_t> compliant {};
		for (size_t j = 0; j < trait_counts.size(); ++j) {
			if (trait_counts.at(j).at(i) > max) {
				max = trait_counts.at(j).at(i);
				// if we set a new maximum, the old overloads don't count
				compliant.clear();
			}
			if (trait_counts.at(j).at(i) >= max) compliant.insert(j);
		}

		// remove all non-compliant overloads from the global compliant set
		std::erase_if(global_compliant, [&compliant](size_t j) { return !compliant.contains(j); });

		// if the global compliant set is empty, we're done
		if (global_compliant.empty()) return global_compliant;
	}

	return global_compliant;
}

bool Resolver::specialize_overload(UndecidedOverload& undecided_overload) {
	// we can only specialize if all candidates are known to satisfy trait bounds
	std::vector<std::vector<size_t>> trait_counts {};
	for (UndecidedOverload::Candidate& candidate : undecided_overload.candidates) {
		trait_counts.push_back({});
		auto const& function_generics = type_pool_.at(candidate.function).get_function().generics;
		auto const& call_generics     = type_pool_.at(candidate.call_id).get_function().generics;
		assert(function_generics.size() == call_generics.size());
		std::unordered_map<TypeInfo::ID, TypeInfo::ID> generic_map {};
		for (size_t i = 0; i < call_generics.size(); ++i) {
			generic_map.insert_or_assign(
				std::get<1>(function_generics.at(i)),
				std::get<1>(call_generics.at(i))
			);
		}
		for (size_t i = 0; i < function_generics.size(); ++i) {
			auto const& function_generic = type_pool_.at(std::get<1>(function_generics.at(i)));
			assert(!function_generic.is_bottom());

			auto const& call_generic_type = type_pool_.at(std::get<1>(call_generics.at(i)));

			auto satisfies = satisfies_trait_constraint(
				call_generic_type.is_generic() ? get_all_constraints(call_generic_type.get_generic())
							       : get_implemented_traits(call_generic_type),
				function_generic.get_generic().declared_constraints,
				generic_map
			);
			if (!satisfies.has_value()) {
				// we must delay this once more
				return false;
			}

			// PERF: we could create a function that counts instead of using actual expensive trait
			// expansion
			std::vector<TypeInfo::Generic::TraitConstraint> constraints {};
			for (auto const& constraint : function_generic.get_generic().declared_constraints) {
				auto expanded_traits = expand_trait(constraint);
				std::move(
					expanded_traits.begin(),
					expanded_traits.end(),
					std::back_inserter(constraints)
				);
			}
			auto reduced_constraints = reduce_to_unique(std::move(constraints));
			if (!reduced_constraints.has_value()) return false;
			trait_counts.back().push_back(reduced_constraints.value().size());
		}
	}

	// since they all do, we will decide based on trait counts
	std::unordered_set<size_t> compliant_set = specialize_overload(std::move(trait_counts));

	// if none are compliant, we don't do anything and just let it be
	if (compliant_set.empty()) return false;

	// if all are compliant, this didn't help us at all
	if (compliant_set.size() == undecided_overload.candidates.size()) return false;

	// if some are compliant, we reduce the set to those
	std::vector<UndecidedOverload::Candidate> new_candidates {};
	for (size_t i = 0; i < undecided_overload.candidates.size(); ++i)
		if (compliant_set.contains(i)) new_candidates.push_back(std::move(undecided_overload.candidates.at(i)));
	undecided_overload.candidates = std::move(new_candidates);

	// function overload resolution will take care of the rest
	return true;
}

bool Resolver::specialize_overload_named_type(TypeInfo::ID id) {
	assert(type_pool_.at(id).is_named());
	auto& candidates = type_pool_.at(id).get_named().candidates();
	// we can only specialize if all candidates are known to satisfy trait bounds
	std::vector<std::vector<size_t>> trait_counts {};
	for (TypeInfo::Named::Candidate& candidate : candidates) {
		trait_counts.push_back({});

		AST::Struct const& struct_ = *std::get<AST::Struct*>(symbol_pool_.at(candidate.name).item);
		// FIXME: instantiating every single time is insane
		auto instantiated_struct = instantiate_struct(&struct_);
		if (instantiated_struct.generic_declaration.empty()) continue;
		auto const& our_generics = candidate.generics;
		assert(instantiated_struct.generic_declaration.size() == our_generics.size());
		std::unordered_map<TypeInfo::ID, TypeInfo::ID> generic_map {};
		for (size_t i = 0; i < our_generics.size(); ++i) {
			generic_map.insert_or_assign(instantiated_struct.generic_declaration.at(i), our_generics.at(i));
		}
		for (size_t i = 0; i < instantiated_struct.generic_declaration.size(); ++i) {
			auto const& instantiated_generic = type_pool_.at(instantiated_struct.generic_declaration.at(i));
			assert(!instantiated_generic.is_bottom());

			auto satisfies = satisfies_trait_constraint(
				our_generics.at(i),
				instantiated_generic.get_generic().declared_constraints,
				generic_map
			);
			if (!satisfies.has_value()) {
				// we must delay this once more
				return false;
			}

			// PERF: we could create a function that counts instead of using actual expensive trait
			// expansion
			std::vector<TypeInfo::Generic::TraitConstraint> constraints {};
			for (auto const& constraint : instantiated_generic.get_generic().declared_constraints) {
				auto expanded_traits = expand_trait(constraint);
				std::move(
					expanded_traits.begin(),
					expanded_traits.end(),
					std::back_inserter(constraints)
				);
			}
			auto reduced_constraints = reduce_to_unique(std::move(constraints));
			if (!reduced_constraints.has_value()) return false;
			trait_counts.back().push_back(reduced_constraints.value().size());
		}
	}

	// since they all do, we will decide based on trait counts
	std::unordered_set<size_t> compliant_set = specialize_overload(std::move(trait_counts));

	// if none are compliant, we don't do anything and just let it be
	if (compliant_set.empty()) return false;

	// if all are compliant, this didn't help us at all
	if (compliant_set.size() == candidates.size()) return false;

	// if some are compliant, we reduce the set to those
	std::vector<TypeInfo::Named::Candidate> new_candidates {};
	for (size_t i = 0; i < candidates.size(); ++i)
		if (compliant_set.contains(i)) new_candidates.push_back(std::move(candidates.at(i)));
	candidates = std::move(new_candidates);

	// if only one is compliant, we have to constrain the type, since try_decide_named_type won't be called!
	if (candidates.size() == 1) constrain_candidate(type_pool_.at(id).get_named().name, candidates.at(0));

	// if more are compliant, this will likely be decided at a later iteration
	return true;
}
