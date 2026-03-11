#include "resolver.hpp"

#include <algorithm>
#include <cstdlib>
#include <iostream>
#include <sstream>
#include <variant>

bool Resolver::try_decide(UndecidedDeref& deref) {
	TypeInfo::ID type_id = deref.inner_type;

	auto decided = type_pool_.at(type_id).is_decided(type_pool_);
	if (decided == -1) goto bottom;
	if (decided == 0) return false;
	while (type_pool_.at(type_id).is_same_as()) {
		auto& ids = type_pool_.at(type_id).get_same_as().ids;
		if (ids.size() != 1) return false;
		type_id = ids.at(0);
	}
	if (type_pool_.at(type_id).is_bottom()) goto bottom;
	if (type_pool_.at(type_id).is_unknown()) return false;
	if (!type_pool_.at(type_id).is_pointer()) {
		parsed_files.at(get_type_file_id(deref.inner_type))
			.diagnostics.push_back(
				Diagnostic::error(
					"type mismatch",
					"only pointers can be dereferenced",
					{get_type_sample(deref.inner_type, OutFmt::Color::Red)}
				)
			);
		goto bottom;
	}

	unify(deref.result_type, type_pool_.at(type_id).get_pointer().pointee, get_type_file_id(deref.result_type));
	return true;
bottom:
	auto btm = register_type(
		TypeInfo::make_bottom(),
		get_type_span(deref.inner_type),
		get_type_file_id(deref.inner_type)
	);
	unify(deref.result_type, btm, get_type_file_id(deref.result_type));
	return true;
}

std::optional<bool>
Resolver::does_overload_candidate_satisfy_trait_bounds(UndecidedOverload::Candidate const& candidate) {
	// we first copy the functions
	// TODO: delete everything afterwards
	TypeInfo::ID function_id = copy_type(candidate.function);
	TypeInfo::ID call_id     = copy_type(candidate.call_id);

	auto const& function      = type_pool_.at(function_id).get_function();
	auto const& call_function = type_pool_.at(call_id).get_function();

	// let's bind the explicit generics
	assert(call_function.generics.size() == function.generics.size());
	std::unordered_map<TypeInfo::ID, TypeInfo::ID> generic_map {};
	for (size_t i = 0; i < call_function.generics.size(); ++i) {
		TypeInfo::ID generic_id          = std::get<1>(call_function.generics.at(i));
		TypeInfo::ID function_generic_id = std::get<1>(function.generics.at(i));

		generic_map.insert_or_assign(generic_id, function_generic_id);
	}
	for (size_t i = 0; i < call_function.generics.size(); ++i) {
		TypeInfo::ID generic_id = std::get<1>(call_function.generics.at(i));
		while (type_pool_.at(generic_id).is_same_as()) {
			auto const& ids = type_pool_.at(generic_id).get_same_as().ids;
			if (ids.size() != 1) break;
			generic_id = ids.at(0);
		}
		// if it's still a SameAs, we don't know it!
		if (type_pool_.at(generic_id).is_same_as()) continue;
		// if it's unknown or a member access, we can't yet decide stuff for this overload
		if (type_pool_.at(generic_id).is_unknown() || type_pool_.at(generic_id).is_member_access()) {
			return std::nullopt;
		}
		// we just skip errors
		if (type_pool_.at(generic_id).is_bottom()) continue;

		// now that we have an actual type, we can constrain the function generic
		TypeInfo::ID function_generic_id = std::get<1>(function.generics.at(i));
		auto&        function_generic    = type_pool_.at(function_generic_id).get_generic();

		if (type_pool_.at(generic_id).is_generic()) {
			// if it's a generic, we can add its declared constraints here
			auto const& declared_constraints = type_pool_.at(generic_id).get_generic().declared_constraints;
			for (auto const& constraint : declared_constraints) {
				function_generic.imposed_constraints.push_back(
					instantiate_constraint(constraint, generic_map)
				);
			}
		} else {
			// if it's just a type, we can add a type constraint
			function_generic.imposed_constraints.push_back(TypeInfo::Generic::TypeConstraint {generic_id});
		}
	}

	// now, let's unify the arguments
	assert(call_function.arguments.size() == function.arguments.size());
	for (size_t i = 0; i < call_function.arguments.size(); ++i) {
		TypeInfo::ID call_argument_id     = std::get<1>(call_function.arguments.at(i));
		TypeInfo::ID function_argument_id = std::get<1>(function.arguments.at(i));
		assert(can_unify(call_argument_id, function_argument_id)
		       && "somehow the functions became un-unifiable");
		UnifyCtx ctx {.generic_map = std::move(generic_map), .leftward = false};
		unify(function_argument_id, call_argument_id, get_type_file_id(call_argument_id), ctx);
		generic_map = std::move(ctx.generic_map);
	}

	// finally, we have collected every trait bound in the cloned function's generics, so we can check them now.
	// we want, for each generic, that its imposed constraints are at least as strict as its declared constraints.
	for (size_t i = 0; i < function.generics.size(); ++i) {
		auto satisfies = can_decide_generic_type(std::get<1>(function.generics.at(i)), generic_map);
		if (!satisfies.has_value()) return std::nullopt;
		if (!satisfies.value()) return false;
	}

	return true;
}

bool Resolver::try_decide(UndecidedOverload& undecided_overload) {
	// filter the candidates
	std::vector<UndecidedOverload::Candidate> new_candidates {};
	for (UndecidedOverload::Candidate& candidate : undecided_overload.candidates) {
		if (!can_unify(candidate.call_id, candidate.function)) {
			// TODO: specify how it is incompatible?
			std::stringstream text {};
			text
				<< "function signature ("
				<< get_type_name(candidate.function)
				<< ") is incompatible with the function call signature ("
				<< get_type_name(candidate.call_id)
				<< ")";
			undecided_overload.rejections.push_back(
				Diagnostic::Sample(
					get_context(get_type_file_id(candidate.function)),
					{Diagnostic::Sample::Label(
						get_type_span(candidate.function),
						text.str(),
						OutFmt::Color::Magenta
					)}
				)
			);
			continue;
		}

		auto satisfies = does_overload_candidate_satisfy_trait_bounds(candidate);
		if (satisfies.has_value() && !satisfies.value()) {
			// TODO: specify exactly which generic does not satisfy trait bounds
			std::stringstream text {};
			text << "does not satisfy trait bounds";
			undecided_overload.rejections.push_back(
				Diagnostic::Sample(
					get_context(get_type_file_id(candidate.function)),
					{Diagnostic::Sample::Label(
						get_type_span(candidate.function),
						text.str(),
						OutFmt::Color::Magenta
					)}
				)
			);

			continue;
		}

		new_candidates.push_back(std::move(candidate));
	}
	undecided_overload.candidates = std::move(new_candidates);

	// if no candidates are unifiable, it's unresolved.
	if (undecided_overload.candidates.empty()) {
		parsed_files.at(undecided_overload.file_id)
			.diagnostics.push_back(
				Diagnostic::error(
					"could not resolve function overload",
					"no function matched the constraints imposed by the function call",
					std::move(undecided_overload.rejections)
				)
			);

		// we need to "unresolve" the callee identifier just in case
		if (undecided_overload.identifier.has_value()) undecided_overload.identifier.value()->id = {};

		// we also need to set the type to bottom to avoid causing more issues
		type_pool_.at(undecided_overload.expr_type) = TypeInfo::make_bottom();

		return true;
	}

	// if too many candidates are unifiable, we fail to decide.
	if (undecided_overload.candidates.size() > 1) { return false; }

	// if only one is unifiable, we've finally found the one and only function
	assert(undecided_overload.candidates.size() == 1);

	constrain_candidate(undecided_overload);

	return true;
}

bool Resolver::try_decide(TypeInfo::ID undecided_member_access) {
	TypeInfo::MemberAccess member_access = type_pool_.at(undecided_member_access).get_member_access();

	// we can only actually decide the member access if we know the type of the accessee
	std::optional<TypeInfo*> maybe_underlying
		= type_pool_.at(member_access.accessee).get_single_underlying(type_pool_);
	if (!maybe_underlying.has_value()) return false;

	// only named types can have fields as of right now
	TypeInfo& underlying = *maybe_underlying.value();
	if (!underlying.is_named()) return false;

	// check that our named type is decided
	int decided = underlying.is_decided(type_pool_);
	if (decided == -1) {
		// if it is impossible, we might have thrown a diagnostic?
		// TODO: have we?
		std::cout << "undecided type (just in case a diagnostic was not thrown): ";
		debug_print_type(undecided_member_access);
		std::cout << std::endl;
		return true;
	} else if (decided == 0) {
		// if it is to be determined, we must wait
		return false;
	}
	// if it is decided, the base is decided
	// this member access will 100% be resolved now! :D

	// let's ensure that we have the proper generic constraints
	AST::SymbolID type_name_id = underlying.get_named().candidates().at(0).name;
	constrain_candidate(underlying.get_named().name, underlying.get_named().candidates().at(0));

	// we don't have any other user type as of now :P
	AST::Struct* struct_ = std::get<AST::Struct*>(symbol_pool_.at(type_name_id).item);

	// first of all, let's check whether the struct even has the field
	auto maybe_field = std::find_if(
		struct_->fields.begin(),
		struct_->fields.end(),
		[&member_access](AST::Struct::Field const& field) { return field.name.value == member_access.field; }
	);
	if (maybe_field == struct_->fields.cend()) {
		// if it doesn't exist, we throw a diagnostic
		FileContext::ID   file_id = get_type_file_id(undecided_member_access);
		std::stringstream subtitle_stream {};
		subtitle_stream
			<< "type `"
			<< struct_->name.value.name()
			<< "` does not have any field named `"
			<< member_access.field
			<< "`";
		parsed_files.at(file_id).diagnostics.push_back(
			Diagnostic::error(
				"tried to access nonexistent field",
				subtitle_stream.str(),
				{Diagnostic::Sample(
					get_context(file_id),
					get_type_span(undecided_member_access),
					OutFmt::Color::Red
				)}
			)
		);

		// we need to set the type to bottom to avoid causing more issues
		type_pool_.at(undecided_member_access) = TypeInfo::make_bottom();
		return true;
	}

	// let's get the type of the field
	AST::Struct::Field& field      = *maybe_field;
	TypeInfo            field_type = from_type(field.type.value, symbol_pool_.at(type_name_id).file_id, false);

	// finally, let's set the type and unify all possible types
	type_pool_.at(undecided_member_access) = field_type;
	for (TypeInfo::ID id : member_access.possible_types) {
		unify(id, undecided_member_access, get_type_file_id(id));
	}

	return true;
}

bool Resolver::try_decide_named_type(TypeInfo::ID id) {
	assert(type_pool_.at(id).is_named());
	auto candidates = std::move(type_pool_.at(id).get_named().candidates());

	// there should be candidates
	assert(!candidates.empty());

	// filter the candidates
	// TODO: rejections
	std::vector<TypeInfo::Named::Candidate> new_candidates {};
	for (TypeInfo::Named::Candidate& candidate : candidates) {
		AST::Struct const& struct_ = *std::get<AST::Struct*>(symbol_pool_.at(candidate.name).item);
		// FIXME: instantiating every single time is insane
		auto instantiated_struct = instantiate_struct(&struct_);
		if (instantiated_struct.generic_declaration.empty()) {
			new_candidates.push_back(std::move(candidate));
			continue;
		}
		auto const& our_generics = candidate.generics;
		assert(instantiated_struct.generic_declaration.size() == our_generics.size());
		std::unordered_map<TypeInfo::ID, TypeInfo::ID> generic_map {};
		for (size_t i = 0; i < our_generics.size(); ++i) {
			generic_map.insert_or_assign(instantiated_struct.generic_declaration.at(i), our_generics.at(i));
		}

		bool satisfies_bounds = true;
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
				// TODO: ensure the named type resolution is actually delayed
				std::cout << "delayed named res!" << std::endl;
				continue;
			}

			if (!satisfies.value()) {
				satisfies_bounds = false;
				break;
			}
		}
		if (!satisfies_bounds) continue;

		new_candidates.push_back(std::move(candidate));
	}
	type_pool_.at(id).get_named().candidates() = std::move(new_candidates);

	// if no candidates are unifiable, it's unresolved.
	if (type_pool_.at(id).get_named().candidates().empty()) {
		// we need to set the type to bottom to avoid causing more issues
		type_pool_.at(id) = TypeInfo::make_bottom();

		// TODO: show rejections instead of just (impossible) here
		parsed_files.at(get_type_file_id(id))
			.diagnostics.push_back(
				Diagnostic::error(
					"could not resolve named type",
					"no named type matched the imposed constraints",
					{get_type_sample(id, OutFmt::Color::Red)}
				)
			);

		return true;
	}

	// if too many candidates are unifiable, we fail to decide.
	if (type_pool_.at(id).get_named().candidates().size() > 1) { return false; }

	// if only one is unifiable, we've finally found the one and only type
	assert(type_pool_.at(id).get_named().candidates().size() == 1);
	constrain_candidate(type_pool_.at(id).get_named().name, type_pool_.at(id).get_named().candidates().at(0));
	return true;
}

std::optional<bool>
Resolver::can_decide_generic_type(TypeInfo::ID id, std::unordered_map<TypeInfo::ID, TypeInfo::ID> const& generic_map) {
	assert(type_pool_.at(id).is_generic());
	auto& generic = type_pool_.at(id).get_generic();

	// all imposed constraints must satisfy the declared constraints
	std::vector<TypeInfo::Generic::TraitConstraint> trait_constraints {};
	std::vector<TypeInfo::Generic::TraitConstraint> trait_and_type_constraints {};
	std::vector<TypeInfo::Generic::TypeConstraint>  type_constraints {};
	// type constraints must satisfy declared trait constraints
	for (auto const& constraint : generic.imposed_constraints) {
		if (std::holds_alternative<TypeInfo::Generic::TypeConstraint>(constraint)) {
			auto const& type_constraint = std::get<TypeInfo::Generic::TypeConstraint>(constraint);

			std::optional<bool> satisfies = satisfies_trait_constraint(
				type_constraint.type,
				generic.declared_constraints,
				generic_map
			);

			if (!satisfies.has_value()) return std::nullopt;
			if (!satisfies.value()) return false;
			type_constraints.push_back(type_constraint);
			auto implemented_traits = get_implemented_traits(type_pool_.at(type_constraint.type));
			std::move(
				implemented_traits.begin(),
				implemented_traits.end(),
				std::back_inserter(trait_and_type_constraints)
			);
		} else {
			trait_constraints.push_back(std::get<TypeInfo::Generic::TraitConstraint>(constraint));
			trait_and_type_constraints.push_back(std::get<TypeInfo::Generic::TraitConstraint>(constraint));
		}
	}
	// trait constraints must satisfy declared trait constraints
	auto satisfies_declared
		= satisfies_trait_constraint(trait_and_type_constraints, generic.declared_constraints, generic_map);
	if (!satisfies_declared.has_value()) return std::nullopt;
	if (!satisfies_declared.value()) return false;

	// if there are no imposed type constraints then there is no concrete type!
	if (type_constraints.empty()) return true;

	// if we do have type constraints, we unify them together
	type_pool_.at(id) = TypeInfo::make_unknown();
	for (size_t i = 0; i < type_constraints.size(); ++i) {
		if (!can_unify(id, type_constraints[i].type)) return false;
		unify(id, type_constraints[i].type, get_type_file_id(id));
	}

	// finally, we check whether the resulting type satisfies the imposed trait constraints
	auto satisfies = satisfies_trait_constraint(id, trait_constraints, generic_map);
	assert(satisfies.has_value()
	       && "there is no way you cannot check whether trait constraints are satisfied by now");
	return satisfies.value();
}

bool Resolver::try_decide_generic_type(TypeInfo::ID id) {
	assert(type_pool_.at(id).is_generic());
	auto& generic = type_pool_.at(id).get_generic();

	// all imposed constraints must satisfy the declared constraints
	bool                                            satisfies_trait_constraints = true;
	std::vector<TypeInfo::Generic::TraitConstraint> trait_constraints {};
	std::vector<TypeInfo::Generic::TypeConstraint>  type_constraints {};
	// type constraints must satisfy declared trait constraints
	for (auto const& constraint : generic.imposed_constraints) {
		if (std::holds_alternative<TypeInfo::Generic::TypeConstraint>(constraint)) {
			auto const& type_constraint = std::get<TypeInfo::Generic::TypeConstraint>(constraint);

			std::optional<bool> satisfies
				= satisfies_trait_constraint(type_constraint.type, generic.declared_constraints);
			if (!satisfies.has_value()) {
				// we cannot yet finish resolving this, because the type is not yet known
				return false;
			}
			if (!satisfies.value()) satisfies_trait_constraints = false;
			type_constraints.push_back(type_constraint);
		} else {
			trait_constraints.push_back(std::get<TypeInfo::Generic::TraitConstraint>(constraint));
		}
	}
	// trait constraints must satisfy declared trait constraints
	auto satisfies_declared = satisfies_trait_constraint(trait_constraints, generic.declared_constraints);
	if (!satisfies_declared.has_value()) return false;
	satisfies_trait_constraints = satisfies_trait_constraints && satisfies_declared.value();
	if (!satisfies_trait_constraints) {
		type_pool_.at(id) = TypeInfo::make_bottom();
		// TODO: rejections list
		parsed_files.at(get_type_file_id(id))
			.diagnostics.push_back(
				Diagnostic::error(
					"type does not satisfy declared trait constraints",
					{Diagnostic::Sample(
						get_context(get_type_file_id(id)),
						get_type_span(id),
						OutFmt::Color::Red
					)}
				)
			);
		return true;
	}

	// if there are no imposed type constraints then there is no concrete type!
	if (type_constraints.empty()) {
		// this is not an issue, though
		return true;
	}

	// if we do have type constraints, we unify them together
	type_pool_.at(id) = TypeInfo::make_unknown();
	for (size_t i = 0; i < type_constraints.size(); ++i) {
		unify(id, type_constraints[i].type, get_type_file_id(id));
	}

	// finally, we check whether the resulting type satisfies the imposed trait constraints
	auto satisfies = satisfies_trait_constraint(id, trait_constraints);
	assert(satisfies.has_value()
	       && "there is no way you cannot check whether trait constraints are satisfied by now");
	if (!satisfies.value()) {
		type_pool_.at(id) = TypeInfo::make_bottom();
		// TODO: rejections list
		parsed_files.at(get_type_file_id(id))
			.diagnostics.push_back(
				Diagnostic::error(
					"type does not satisfy imposed trait constraints",
					{Diagnostic::Sample(
						get_context(get_type_file_id(id)),
						get_type_span(id),
						OutFmt::Color::Red
					)}
				)
			);
	}

	return true;
}

bool Resolver::check_generic_type(TypeInfo::ID id) {
	assert(type_pool_.at(id).is_generic());
	auto& generic = type_pool_.at(id).get_generic();

	// all declared constraints must satisfy the imposed constraints
	std::vector<TypeInfo::Generic::TraitConstraint> trait_constraints {};
	// if there is any type constraint, we bail
	// TODO: there might be an edge case where a type is the only implementer of a trait and therefore a type
	// constraint makes sense?
	for (auto const& constraint : generic.imposed_constraints) {
		if (std::holds_alternative<TypeInfo::Generic::TypeConstraint>(constraint)) {
			// TODO: diagnostic
			return true;
		} else trait_constraints.push_back(std::get<TypeInfo::Generic::TraitConstraint>(constraint));
	}
	auto satisfies = satisfies_trait_constraint(generic.declared_constraints, trait_constraints);
	if (!satisfies.has_value()) return false;
	if (!satisfies.value()) {
		type_pool_.at(id) = TypeInfo::make_bottom();
		// TODO: rejections list, and better diagnostic??
		parsed_files.at(get_type_file_id(id))
			.diagnostics.push_back(
				Diagnostic::error(
					"type is not capable enough",
					{Diagnostic::Sample(
						get_context(get_type_file_id(id)),
						get_type_span(id),
						OutFmt::Color::Red
					)}
				)
			);
	}

	return true;
}
