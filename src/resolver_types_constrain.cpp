#include "resolver.hpp"

#include <cstdlib>
#include <iostream>

void Resolver::constrain_candidate(UndecidedOverload& undecided_overload) {
	assert(undecided_overload.candidates.size() == 1);
	auto& candidate = undecided_overload.candidates.at(0);

	// now we must constrain the candidate
	FileContext::ID file_id = get_type_file_id(candidate.call_id);

	assert(type_pool_.at(candidate.call_id).is_function() && type_pool_.at(candidate.function).is_function());

	TypeInfo::Function &call      = type_pool_.at(candidate.call_id).get_function(),
			   &prototype = type_pool_.at(candidate.function).get_function();

	assert(call.arguments.size() == prototype.arguments.size());
	assert(call.generics.size() == prototype.generics.size());

	std::unordered_map<TypeInfo::ID, TypeInfo::ID> generic_map {};
	for (size_t i = 0; i < call.generics.size(); ++i) {
		generic_map.insert_or_assign(std::get<1>(prototype.generics.at(i)), std::get<1>(call.generics.at(i)));
	}

	for (size_t i = 0; i < call.generics.size(); ++i) {
		if (!type_pool_.at(std::get<1>(call.generics.at(i))).is_generic()) continue;
		auto& call_generic      = type_pool_.at(std::get<1>(call.generics.at(i))).get_generic();
		auto& prototype_generic = type_pool_.at(std::get<1>(prototype.generics.at(i))).get_generic();

		for (auto const& constraint : prototype_generic.declared_constraints) {
			call_generic.imposed_constraints.push_back(instantiate_constraint(constraint, generic_map));
		}
	}

	// we create a new prototype with the generics all replaced
	auto const& new_prototype = type_pool_.at(instantiate_type(candidate.function, generic_map)).get_function();

	// unify args
	for (size_t i = 0; i < call.arguments.size(); ++i) {
		TypeInfo::ID call_arg_type      = std::get<1>(call.arguments.at(i));
		TypeInfo::ID prototype_arg_type = std::get<1>(new_prototype.arguments.at(i));

		unify(call_arg_type, prototype_arg_type, file_id);
	}

	// unify return type
	unify(call.return_, new_prototype.return_, file_id);

	// if we're calling an identifier, let's finish resolving it
	if (undecided_overload.identifier.has_value()) {
		if (!type_symbol_mapping_.at(candidate.function).has_value()) {
			// TODO: think about when this would ever happen
			std::cout << "error: there is no value for this call ID: ";
			debug_print_type(candidate.function);
			std::cout << std::endl;
		} else {
			undecided_overload.identifier.value()->id
				= {type_symbol_mapping_.at(candidate.function).value()};
		}
	}

	undecided_overload.function_call->call_type = {candidate.call_id};
}

void Resolver::constrain_candidate(AST::Identifier* identifier, TypeInfo::Named::Candidate& candidate) {
	// constrain the identifier for lowering later
	if (identifier) identifier->id = {candidate.name};

	AST::Struct* struct_             = std::get<AST::Struct*>(get_single_symbol(candidate.name).item);
	auto         instantiated_struct = instantiate_struct(struct_);
	if (instantiated_struct.generic_declaration.empty()) return;
	auto& generics          = candidate.generics;
	auto& declared_generics = struct_->generic_declaration.value().generics;

	assert(generics.size() == declared_generics.size()
	       && declared_generics.size() == instantiated_struct.generic_declaration.size());

	std::unordered_map<TypeInfo::ID, TypeInfo::ID> generic_map {};
	for (size_t i = 0; i < generics.size(); ++i) {
		generic_map.insert_or_assign(instantiated_struct.generic_declaration.at(i), generics.at(i));
	}

	// add trait constraints from the struct generics
	for (size_t i = 0; i < generics.size(); ++i) {
		ensure_has_constraints(declared_generics.at(i), get_single_symbol(candidate.name).file_id);
		auto generic    = instantiated_struct.generic_declaration.at(i);
		auto generified = generify_type(generics.at(i), declared_generics.at(i).name.value.id.value()[0]);

		for (auto const& constraint : type_pool_.at(generic).get_generic().declared_constraints) {
			type_pool_.at(generified)
				.get_generic()
				.imposed_constraints.push_back(instantiate_constraint(constraint, generic_map));
		}

		generics.at(i) = generified;
	}
}
