#include "resolver.hpp"

#include <algorithm>
#include <cstdlib>

bool Resolver::specialize_overloads() {
	bool any_succeeded = false;
	// specialize functions
	for (UndecidedOverload& undecided_overload : undecided_overloads)
		any_succeeded = any_succeeded || specialize_overload(undecided_overload);

	// specialize types
	for (TypeInfo::ID id = 0; id < type_pool_.size(); ++id) {
		TypeInfo const& type = type_pool_.at(id);
		if (!type.is_named()) continue;
		int decided = type.is_decided(type_pool_);
		if (decided != 0) continue;
		any_succeeded = any_succeeded || specialize_overload_named_type(id);
	}

	return any_succeeded;
}

void Resolver::decide_supposedly_known_named_types() {
	for (TypeInfo::ID id = 0; id < type_pool_.size(); ++id) {
		TypeInfo& type = type_pool_.at(id);
		if (!type.is_named()) continue;
		if (type.get_named().candidates().size() != 1) continue;
		try_decide_named_type(id);
	}
}

bool Resolver::has_undecided_named_types() const {
	for (TypeInfo::ID id = 0; id < type_pool_.size(); ++id) {
		TypeInfo const& type = type_pool_.at(id);
		if (!type.is_named()) continue;
		int decided = type.is_decided(type_pool_);
		if (decided == 0) return true;
	}
	return false;
}

bool Resolver::try_decide_remaining_types() {
	// PERF: use std::copy_if
	while (!undecided_overloads.empty()
	       || !undecided_member_accesses.empty()
	       || !undecided_generics.empty()
	       || !unchecked_generics.empty()
	       || !undecided_derefs.empty()
	       || has_undecided_named_types()) {
		bool any_succeeded = false;

		// we first try to decide the overloads
		std::vector<UndecidedOverload> remaining_overloads {};
		for (UndecidedOverload& undecided_overload : undecided_overloads)
			if (!try_decide(undecided_overload))
				remaining_overloads.push_back(std::move(undecided_overload));
		size_t old_size     = undecided_overloads.size();
		undecided_overloads = std::move(remaining_overloads);
		if (old_size != undecided_overloads.size()) any_succeeded = true;

		// then we try to decide the member accesses
		std::vector<TypeInfo::ID> remaining_member_accesses {};
		for (TypeInfo::ID undecided_member_access : undecided_member_accesses)
			if (!try_decide(undecided_member_access))
				remaining_member_accesses.push_back(undecided_member_access);
		old_size                  = undecided_member_accesses.size();
		undecided_member_accesses = std::move(remaining_member_accesses);
		if (old_size != undecided_member_accesses.size()) any_succeeded = true;

		// then we try to decide the named types
		for (TypeInfo::ID id = 0; id < type_pool_.size(); ++id) {
			TypeInfo const& type = type_pool_.at(id);
			if (!type.is_named()) continue;
			int decided = type.is_decided(type_pool_);
			if (decided != 0) continue;
			if (try_decide_named_type(id)) any_succeeded = true;
		}

		// then we try to decide generics
		std::vector<TypeInfo::ID> remaining_undecided_generics {};
		for (TypeInfo::ID undecided_generic : undecided_generics)
			if (!try_decide_generic_type(undecided_generic))
				remaining_undecided_generics.push_back(undecided_generic);
		old_size           = undecided_generics.size();
		undecided_generics = std::move(remaining_undecided_generics);
		if (old_size != undecided_generics.size()) any_succeeded = true;

		// then we try to check generics
		std::vector<TypeInfo::ID> remaining_unchecked_generics {};
		for (TypeInfo::ID unchecked_generic : unchecked_generics)
			if (!check_generic_type(unchecked_generic))
				remaining_unchecked_generics.push_back(unchecked_generic);
		old_size           = unchecked_generics.size();
		unchecked_generics = std::move(remaining_unchecked_generics);
		if (old_size != unchecked_generics.size()) any_succeeded = true;

		// then we try to decide derefs
		std::vector<UndecidedDeref> remaining_undecided_derefs {};
		for (UndecidedDeref& undecided_deref : undecided_derefs)
			if (!try_decide(undecided_deref))
				remaining_undecided_derefs.push_back(std::move(undecided_deref));
		old_size         = undecided_derefs.size();
		undecided_derefs = std::move(remaining_undecided_derefs);
		if (old_size != undecided_derefs.size()) any_succeeded = true;

		// finally, we quit if we made no progress
		if (!any_succeeded) break;
	}

	// only if all arrays are empty we have finished deciding the program
	return undecided_overloads.empty()
	    && undecided_member_accesses.empty()
	    && undecided_generics.empty()
	    && unchecked_generics.empty()
	    && undecided_derefs.empty()
	    && !has_undecided_named_types();
}

void Resolver::decide_remaining_types() {
	do {
		// we try to decide remaining overloads and member accesses now that the entire program is known
		if (try_decide_remaining_types()) return;
	} while (specialize_overloads());  // we specialize overloads on fail

	// we will try a more destructive approach now: we will fill in all partial numeric types and try to decide.
	// this doesn't really have an effect if it doesn't work, because the lowering phase would have done this
	// anyway!
	// TODO: abstract this functionality and do it for all partial numeric types
	bool changes_made = false;
	for (UndecidedOverload& undecided_overload : undecided_overloads) {
		for (UndecidedOverload::Candidate& candidate : undecided_overload.candidates) {
			for (auto& [_, id] : type_pool_.at(candidate.call_id).get_function().arguments) {
				auto& type = type_pool_.at(id);
				if (type.is_partial_integer()) {
					auto& partial_integer = type.get_partial_integer();
					// we will fill in to a signed integer of default width
					bool signed_ = partial_integer.signed_is_known
					                     ? partial_integer.integer.is_signed()
					                     : true;
					std::optional<AST::Type::Atom::Integer> full_integer = std::nullopt;
					switch (partial_integer.integer.width_type()) {
					case AST::Type::Atom::Integer::WidthType::Fixed:
						full_integer = AST::Type::Atom::Integer::with_width(
								       partial_integer.integer.bit_width().value(),
								       signed_
						)
						                       .value();
						break;
					case AST::Type::Atom::Integer::WidthType::Any:
						full_integer = AST::Type::Atom::Integer::with_width(
							IR::DEFAULT_INTEGER_WIDTH,
							signed_
						);
						break;
					case AST::Type::Atom::Integer::WidthType::Ptr:
						full_integer = AST::Type::Atom::Integer::ptr(signed_);
						break;
					case AST::Type::Atom::Integer::WidthType::Size:
						full_integer = AST::Type::Atom::Integer::size(signed_);
						break;
					}
					type = TypeInfo::make_known_integer(
						TypeInfo::KnownInteger {std::move(full_integer.value())}
					);
					changes_made = true;
				} else if (type.is_partial_float()) {
					type = TypeInfo::make_known_float(
						TypeInfo::KnownFloat {
							(AST::Type::Atom::Float::Width) IR::DEFAULT_FLOAT_WIDTH
						}
					);
					changes_made = true;
				}
			}
		}
	}

	// if no type was filled in, there is no hope left
	if (changes_made) {
		// if any type was filled in, though, let's try again!
		// FIXME: this breaks overloads that were determined to be impossible and overloads that were determined
		// to be a single one (e.g. sint vs uint). it also has the potential to prune overloads that would
		// actually be valid after replacing int types by concrete int types! a potential fix is to delay
		// candidate constraining until we know whether everything is solved, and if everything is not solved,
		// we rollback and redo it with partial int types replaced.
		do {
			if (try_decide_remaining_types()) return;
		} while (specialize_overloads());
	}

	// if we did not manage to decide any overload, we gotta throw diagnostics (we literally tried everything we can
	// at this point :P)
	if (!undecided_overloads.empty()) {
		for (UndecidedOverload const& undecided_overload : undecided_overloads) {
			std::vector<Diagnostic::Sample> samples = {undecided_overload.rejections.at(0)};

			size_t count = 0;
			std::transform(
				undecided_overload.candidates.cbegin(),
				undecided_overload.candidates.cend(),
				std::back_inserter(samples),
				[this, &count](UndecidedOverload::Candidate const& candidate) {
					return Diagnostic::Sample(
						get_context(get_type_file_id(candidate.function)),
						std::format("candidate #{}", ++count),
						{Diagnostic::Sample::Label(
							get_type_span(candidate.function),
							OutFmt::Color::Cyan
						)}
					);
				}
			);

			parsed_files.at(undecided_overload.file_id)
				.diagnostics.push_back(
					Diagnostic::error(
						"could not resolve function overload",
						"more than one function matches the function call, so it must be manually disambiguated",
						std::move(samples)
					)
				);

			// we need to "unresolve" the callee identifier just in case
			if (undecided_overload.identifier.has_value()) undecided_overload.identifier.value()->id = {};
		}
	}

	if (!undecided_member_accesses.empty()) {
		for (TypeInfo::ID undecided_member_access : undecided_member_accesses) {
			// we need to determine what led to the member access to not be decided. there are two
			// possibilities
			TypeInfo::MemberAccess& member_access
				= type_pool_.at(undecided_member_access).get_member_access();
			Diagnostic::Sample sample  = get_type_sample(member_access.accessee, OutFmt::Color::Red);
			FileContext::ID    file_id = get_type_file_id(undecided_member_access);

			// the first is that there is not a single underlying type
			if (!type_pool_.at(member_access.accessee).get_single_underlying(type_pool_).has_value()) {
				parsed_files.at(file_id).diagnostics.push_back(
					Diagnostic::error(
						"could not resolve member access",
						"the underlying type of the accessee could not be decided",
						{std::move(sample)}
					)
				);
				continue;
			}

			// the second is that the type cannot have fields
			parsed_files.at(file_id).diagnostics.push_back(
				Diagnostic::error(
					"could not resolve member access",
					"the underlying type of the accessee does not have any fields",
					{std::move(sample)}
				)
			);
		}
	}

	if (has_undecided_named_types()) {
		for (TypeInfo::ID id = 0; id < type_pool_.size(); ++id) {
			TypeInfo const& type = type_pool_.at(id);
			if (!type.is_named()) continue;
			int decided = type.is_decided(type_pool_);
			if (decided != 0) continue;
			parsed_files.at(get_type_file_id(id))
				.diagnostics.push_back(
					Diagnostic::error(
						"could not decide named type",
						"the type cannot be decided between its candidates",
						{get_type_sample(id, OutFmt::Color::Red)}
					)
				);
			type_pool_.at(id) = TypeInfo::make_bottom();
		}
	}

	// TODO: diagnostic for undecided/unchecked generics
}
