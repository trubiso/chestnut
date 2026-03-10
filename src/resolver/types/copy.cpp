#include "resolver.hpp"

#include <algorithm>

Resolver::TypeInfo::ID Resolver::copy_type(TypeInfo::ID id) {
	std::unordered_map<TypeInfo::ID, TypeInfo::ID> generic_map {};
	return copy_type(id, generic_map);
}

Resolver::TypeInfo::ID
Resolver::copy_type(TypeInfo::ID id, std::unordered_map<TypeInfo::ID, TypeInfo::ID>& generic_map) {
	if (generic_map.contains(id)) return generic_map.at(id);

	auto const& type = type_pool_.at(id);

	auto span    = get_type_span(id);
	auto file_id = get_type_file_id(id);
	auto symbol  = type_symbol_mapping_.at(id);

	switch (type_pool_.at(id).kind()) {
	case TypeInfo::Kind::KnownVoid:
	case TypeInfo::Kind::KnownChar:
	case TypeInfo::Kind::KnownBool:
	case TypeInfo::Kind::KnownInteger:
	case TypeInfo::Kind::KnownFloat:
	case TypeInfo::Kind::Module:
	case TypeInfo::Kind::Bottom:
	case TypeInfo::Kind::Unknown:
	case TypeInfo::Kind::PartialInteger:
	case TypeInfo::Kind::PartialFloat:   {
		TypeInfo type_copy = type;
		return register_type(std::move(type_copy), span, file_id);
	}
	case TypeInfo::Kind::Function:
	case TypeInfo::Kind::SameAs:
	case TypeInfo::Kind::Generic:
	case TypeInfo::Kind::MemberAccess:
	case TypeInfo::Kind::Named:
	case TypeInfo::Kind::Pointer:      break;
	}

	if (type.is_function()) {
		std::vector<std::tuple<std::optional<std::string>, TypeInfo::ID>> new_generics {};
		new_generics.reserve(type_pool_.at(id).get_function().generics.size());
		std::transform(
			type_pool_.at(id).get_function().generics.cbegin(),
			type_pool_.at(id).get_function().generics.cend(),
			std::back_inserter(new_generics),
			[this, &generic_map](auto const& generic) {
				return std::tuple {std::get<0>(generic), copy_type(std::get<1>(generic), generic_map)};
			}
		);
		std::vector<std::tuple<std::optional<std::string>, TypeInfo::ID>> new_arguments {};
		new_arguments.reserve(type_pool_.at(id).get_function().arguments.size());
		std::transform(
			type_pool_.at(id).get_function().arguments.cbegin(),
			type_pool_.at(id).get_function().arguments.cend(),
			std::back_inserter(new_arguments),
			[this, &generic_map](
				auto const& argument
			) { return std::tuple {std::get<0>(argument), copy_type(std::get<1>(argument), generic_map)}; }
		);
		return register_type(
			TypeInfo::make_function(
				TypeInfo::Function {
					std::move(new_arguments),
					std::move(new_generics),
					copy_type(type_pool_.at(id).get_function().return_, generic_map)
				}
			),
			span,
			file_id,
			symbol
		);
	} else if (type.is_same_as()) {
		return register_type(
			TypeInfo::make_same_as(copy_types(type.get_same_as().ids, generic_map)),
			span,
			file_id,
			symbol
		);
	} else if (type.is_generic()) {
		// we create the generic before its constraints so its constraints can use it!
		TypeInfo::ID generic_id = register_type(
			TypeInfo::make_generic(TypeInfo::Generic {type.get_generic().name, {}, {}}),
			span,
			file_id,
			symbol
		);

		// we insert it into the generic map
		assert(std::get<1>(generic_map.insert_or_assign(id, generic_id)) && "somehow re-added a generic");

		// now we instantiate the declared constraints
		std::vector<TypeInfo::Generic::TraitConstraint> declared_constraints {};
		declared_constraints.reserve(type_pool_.at(id).get_generic().declared_constraints.size());
		std::transform(
			type_pool_.at(id).get_generic().declared_constraints.cbegin(),
			type_pool_.at(id).get_generic().declared_constraints.cend(),
			std::back_inserter(declared_constraints),
			[this, &generic_map](TypeInfo::Generic::TraitConstraint const& constraint) {
				return TypeInfo::Generic::TraitConstraint {
					constraint.name,
					copy_types(constraint.arguments, generic_map)
				};
			}
		);

		// we add the declared constraints to the generic type
		type_pool_.at(generic_id).get_generic().declared_constraints = std::move(declared_constraints);

		// and finally we return the type
		return generic_id;
	} else if (type.is_member_access()) {
		assert(false && "TODO: copy member access");
	} else if (type.is_named()) {
		assert(!type.get_named().is_partial());
		std::vector<TypeInfo::Named::Candidate> new_candidates {};
		new_candidates.reserve(type.get_named().candidates().size());
		std::transform(
			type_pool_.at(id).get_named().candidates().cbegin(),
			type_pool_.at(id).get_named().candidates().cend(),
			std::back_inserter(new_candidates),
			[this, &generic_map](TypeInfo::Named::Candidate const& candidate) {
				return TypeInfo::Named::Candidate {
					candidate.name,
					copy_types(candidate.generics, generic_map)
				};
			}
		);
		// we don't want to link it back to the same identifier, since this is just a copy!
		return register_type(TypeInfo::make_named(nullptr, std::move(new_candidates)), span, file_id, symbol);
	} else if (type.is_pointer()) {
		return register_type(
			TypeInfo::make_pointer(
				TypeInfo::Pointer {
					copy_type(type_pool_.at(id).get_pointer().pointee, generic_map),
					type_pool_.at(id).get_pointer().mutable_
				}
			),
			span,
			file_id,
			symbol
		);
	}
	[[assume(false)]];
	return id;
}

std::vector<Resolver::TypeInfo::ID> Resolver::copy_types(
	std::vector<TypeInfo::ID> const&                types,
	std::unordered_map<TypeInfo::ID, TypeInfo::ID>& generic_map
) {
	std::vector<TypeInfo::ID> new_types {};
	new_types.reserve(types.size());
	std::transform(
		types.cbegin(),
		types.cend(),
		std::back_inserter(new_types),
		[this, &generic_map](TypeInfo::ID id) { return copy_type(id, generic_map); }
	);
	return new_types;
}
