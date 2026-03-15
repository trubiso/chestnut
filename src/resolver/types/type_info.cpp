#include "resolver.hpp"

#include <algorithm>
#include <cstdlib>
#include <variant>

Resolver::TypeInfo Resolver::from_partial(TypeInfo::Named&& named, Span span, FileContext::ID file_id) {
	auto const& partial = std::get<TypeInfo::Named::Partial>(named.value);
	// first, we ignore name resolver issues
	if (!named.name->has_at_least_one_id()) return TypeInfo::make_bottom();

	std::vector<AST::SymbolID>              candidate_ids = named.name->ids();
	std::vector<TypeInfo::Named::Candidate> candidates {};

	// TODO: rejections list for diagnostic
	bool any_generic_candidate = false;
	for (AST::SymbolID candidate : candidate_ids) {
		if (std::holds_alternative<Generic>(symbol_pool_.at(candidate).item)) {
			// generics cannot take in generics
			if (!partial.ordered_generics.empty() || !partial.labeled_generics.empty()) continue;
			// otherwise, we have a generic candidate
			candidates.emplace_back(candidate, std::vector<TypeInfo::ID> {});
			any_generic_candidate = true;
		}
		if (!std::holds_alternative<AST::Struct*>(symbol_pool_.at(candidate).item)) continue;
		AST::Struct const& struct_ = *std::get<AST::Struct*>(symbol_pool_.at(candidate).item);

		if (partial.ordered_generics.empty() && partial.labeled_generics.empty()) {
			// if we don't have any generics, the struct must have no generics
			if (struct_.generic_declaration.has_value()
			    && !struct_.generic_declaration.value().generics.empty())
				continue;

			candidates.emplace_back(candidate, std::vector<TypeInfo::ID> {});
			continue;
		}

		// if we do have generics, the struct must have the same quantity
		if (!struct_.generic_declaration.has_value()) continue;
		auto const& struct_generics = struct_.generic_declaration.value().generics;
		// TODO: default generics (default everything to inferred by default)
		size_t generic_count = partial.ordered_generics.size() + partial.labeled_generics.size();
		if (generic_count != struct_generics.size()) continue;

		// check that all labeled generics exist, ignoring ordered ones
		std::vector<std::string_view> under_consideration {};
		under_consideration.reserve(struct_generics.size() - partial.ordered_generics.size());
		for (size_t i = partial.ordered_generics.size(); i < struct_generics.size(); ++i)
			if (!struct_generics.at(i).anonymous)
				under_consideration.push_back(struct_generics.at(i).name.value.name);

		if (std::any_of(
			    partial.labeled_generics.cbegin(),
			    partial.labeled_generics.cend(),
			    [&under_consideration](auto const& generic) {
				    auto const& name = std::get<0>(generic);
				    return !std::any_of(
					    under_consideration.cbegin(),
					    under_consideration.cend(),
					    [&name](std::string_view actual) { return name == actual; }
				    );
			    }
		    ))
			continue;

		// this is a match! reconstruction time
		std::vector<TypeInfo::ID> generics {};
		generics.reserve(generic_count);
		std::move(
			partial.ordered_generics.begin(),
			partial.ordered_generics.end(),
			std::back_inserter(generics)
		);
		for (size_t i = generics.size(); i < struct_generics.size(); ++i) {
			auto corresponding_generic = std::find_if(
				partial.labeled_generics.cbegin(),
				partial.labeled_generics.cend(),
				[&struct_generics, i](auto const& generic) {
					return std::get<0>(generic) == struct_generics.at(i).name.value.name;
				}
			);
			generics.push_back(std::get<1>(*corresponding_generic));
		}

		candidates.emplace_back(candidate, std::move(generics));
	}

	if (candidates.empty()) {
		parsed_files.at(file_id).diagnostics.push_back(
			Diagnostic::error(
				"could not resolve type",
				partial.labeled_generics.empty() && partial.ordered_generics.empty()
					? "there is no type with the provided name"
					: "there is no type with the provided name and generic list",
				{Diagnostic::Sample(get_context(file_id), span, OutFmt::Color::Red)}
			)
		);
		return TypeInfo::make_bottom();
	}

	if (any_generic_candidate) {
		assert(candidates.size() == 1);
		return TypeInfo::make_same_as(symbol_pool_.at(candidates.at(0).name).type);
	}
	return TypeInfo::make_named(named.name, std::move(candidates));
}

Resolver::TypeInfo Resolver::from_type(AST::Type::Atom::Named& named, FileContext::ID file_id, bool partial) {
	// for named types, we need to extract the generic list first
	std::vector<TypeInfo::ID>                          ordered_generics {};
	std::vector<std::tuple<std::string, TypeInfo::ID>> labeled_generics {};
	// TODO: deal with generics

	TypeInfo::Named partial_named {
		&named.name.value,
		TypeInfo::Named::Partial {std::move(ordered_generics), std::move(labeled_generics)}
	};

	// resolve the partial only if this is not a partial scenario
	return partial ? TypeInfo {std::move(partial_named)}
	               : from_partial(std::move(partial_named), named.name.span, file_id);
}

Resolver::TypeInfo Resolver::from_type(AST::Type::Atom& atom, FileContext::ID file_id, bool partial) {
	switch (atom.kind()) {
	case AST::Type::Atom::Kind::Float:
		return TypeInfo::make_known_float(TypeInfo::KnownFloat {atom.get_float().width});
	case AST::Type::Atom::Kind::Void:     return TypeInfo::make_known_void();
	case AST::Type::Atom::Kind::Char:     return TypeInfo::make_known_char();
	case AST::Type::Atom::Kind::Bool:     return TypeInfo::make_known_bool();
	case AST::Type::Atom::Kind::Named:    return from_type(atom.get_named(), file_id, partial);
	case AST::Type::Atom::Kind::Inferred: return TypeInfo::make_unknown();
	case AST::Type::Atom::Kind::Integer:  break;
	}

	// for integers, we need to determine how much information we know
	AST::Type::Atom::Integer const& integer = atom.get_integer();
	if (integer.width_type() != AST::Type::Atom::Integer::WidthType::Any)
		return TypeInfo::make_known_integer(TypeInfo::KnownInteger {integer});
	else return TypeInfo::make_partial_integer(TypeInfo::PartialInteger {integer, true});
}

Resolver::TypeInfo Resolver::from_type(AST::Type::Pointer& pointer, FileContext::ID file_id, bool partial) {
	TypeInfo     inner   = from_type(pointer.type->value, file_id, partial);
	TypeInfo::ID pointee = register_type(std::move(inner), pointer.type->span, file_id);
	return TypeInfo::make_pointer(TypeInfo::Pointer {pointee, pointer.mutable_});
}

Resolver::TypeInfo Resolver::from_type(AST::Type& type, FileContext::ID file_id, bool partial) {
	switch (type.kind()) {
	case AST::Type::Kind::Atom:    return from_type(type.get_atom(), file_id, partial);
	case AST::Type::Kind::Pointer: return from_type(type.get_pointer(), file_id, partial);
	}
	[[assume(false)]];
}

bool Resolver::TypeInfo::is_callable(std::vector<TypeInfo> const& pool) const {
	switch (kind()) {
	case Kind::Function: return true;
	case Kind::SameAs:
		for (TypeInfo::ID id : get_same_as().ids)
			if (pool.at(id).is_callable(pool)) return true;
		return false;
	default: return false;
	}
}

bool Resolver::TypeInfo::is_pointer(std::vector<TypeInfo> const& pool) const {
	switch (kind()) {
	case Kind::Pointer: return true;
	case Kind::SameAs:
		// TODO: maybe deal with ambiguous derefs
		if (get_same_as().ids.size() != 1) return false;
		return pool.at(get_same_as().ids.at(0)).is_pointer(pool);
	default: return false;
	}
}

std::vector<Resolver::TypeInfo::ID>
Resolver::TypeInfo::get_callable_subitems(TypeInfo::ID self_id, std::vector<TypeInfo> const& pool) const {
	switch (kind()) {
	case Kind::Function: return {self_id};
	case Kind::SameAs:   break;
	default:             return {};
	}

	// for SameAs, we just collect all the children
	std::vector<ID> ids {};
	for (TypeInfo::ID id : get_same_as().ids) {
		std::vector<ID> subids = pool.at(id).get_callable_subitems(id, pool);
		std::copy(subids.cbegin(), subids.cend(), std::back_inserter(ids));
	}

	return ids;
}

Resolver::TypeInfo::ID Resolver::TypeInfo::get_pointee(std::vector<TypeInfo> const& pool) const {
	assert(is_pointer(pool));
	switch (kind()) {
	case Kind::Pointer: return get_pointer().pointee;
	case Kind::SameAs:  return pool.at(get_same_as().ids.at(0)).get_pointee(pool);
	default:            [[assume(false)]]; return {};
	}
}

bool Resolver::TypeInfo::get_pointer_mutable(std::vector<TypeInfo> const& pool) const {
	assert(is_pointer(pool));
	switch (kind()) {
	case Kind::Pointer: return get_pointer().mutable_;
	case Kind::SameAs:  return pool.at(get_same_as().ids.at(0)).get_pointer_mutable(pool);
	default:            [[assume(false)]]; return {};
	}
}

std::optional<Resolver::TypeInfo*> Resolver::TypeInfo::get_single_underlying(std::vector<TypeInfo>& pool) {
	if (is_same_as()) {
		auto const& ids = get_same_as().ids;
		if (ids.size() != 1) return {};
		return pool.at(ids.at(0)).get_single_underlying(pool);
	}

	return this;
}

int Resolver::TypeInfo::is_decided(std::vector<TypeInfo> const& pool) const {
	if (is_named()) {
		size_t possibilities = get_named().candidates().size();
		// if there is more than one possibility, this is to be determined
		if (possibilities > 1) return 0;
		// if there is no possibility, this is impossible
		if (possibilities < 1) return -1;
		// there is now one single candidate
		auto const& candidate = get_named().candidates().at(0);
		for (TypeInfo::ID generic : candidate.generics) {
			int decided = pool.at(generic).is_decided(pool);
			// if any generic is not decided, so is this named type
			if (decided != 1) return decided;
		}
		// if all generics are decided, we are decided
		return 1;
	}

	if (is_same_as()) {
		auto const& ids = get_same_as().ids;
		// if there is more than one possibility, this is to be determined
		if (ids.size() > 1) return 0;
		// if there is no possibility, this is impossible
		if (ids.size() < 1) return -1;
		// if there is a single possibility, we check it
		return pool.at(ids.at(0)).is_decided(pool);
	}

	if (is_unknown() || is_member_access()) return false;

	// any other type is decided
	return 1;
}
