#include "resolver.hpp"

#include <algorithm>
#include <cstdlib>
#include <sstream>

void Resolver::set_same_as(TypeInfo::ID to, TypeInfo::ID from) {
	// set_same_as(a, a) is a noop
	if (to == from) return;
	// the normal case works as always
	if (!type_pool_.at(from).is_same_as()) {
		type_pool_.at(to) = TypeInfo::make_same_as(from);
		return;
	}
	// for a single candidate, we simply go through and check what it is
	if (type_pool_.at(from).get_same_as().ids.size() == 1) {
		set_same_as(to, type_pool_.at(from).get_same_as().ids.at(0));
		return;
	}
	// for many candidates, we don't do anything yet
	// TODO: filter ourselves out from this pool
	type_pool_.at(to) = type_pool_.at(from);
}

Resolver::TypeInfo Resolver::unify_follow_references(
	TypeInfo::ID    same_as,
	TypeInfo::ID    other,
	TypeInfo::ID    same_as_origin,
	TypeInfo::ID    other_origin,
	FileContext::ID file_id
) {
	assert(type_pool_.at(same_as).is_same_as());
	std::vector<TypeInfo::ID> const& ids = type_pool_.at(same_as).get_same_as().ids;

	// if we have a single id, unify it as normal
	if (ids.size() == 1) {
		unify(ids[0], other, same_as_origin, other_origin, file_id);
		return TypeInfo::make_same_as(ids[0]);
	}

	// if we have more than one, filter the non-unifiable ones out
	std::vector<TypeInfo::ID> new_ids {};
	std::copy_if(ids.cbegin(), ids.cend(), std::back_inserter(new_ids), [this, other](TypeInfo::ID id) {
		return can_unify(id, other);
	});

	// if none can be unified, throw a diagnostic and return Bottom
	if (new_ids.empty()) {
		std::stringstream subtitle_stream {};
		subtitle_stream
			<< "none of the candidates for the type "
			<< get_type_name(same_as)
			<< " are compatible with the type "
			<< get_type_name(other);
		parsed_files.at(file_id).diagnostics.push_back(
			Diagnostic::error(
				"type mismatch",
				subtitle_stream.str(),
				{get_type_sample(same_as_origin, OutFmt::Color::Cyan),
		                 get_type_sample(other_origin, OutFmt::Color::Yellow)}
			)
		);
		return TypeInfo::make_bottom();
	}

	// unify all remaining ids and create a new SameAs
	for (TypeInfo::ID new_id : new_ids) unify(new_id, other, same_as_origin, other_origin, file_id);
	return TypeInfo::make_same_as(std::move(new_ids));
}

bool Resolver::unify_basic_known(
	TypeInfo::Kind  kind,
	TypeInfo::ID    a_id,
	TypeInfo::ID    b_id,
	TypeInfo::ID    a_origin,
	TypeInfo::ID    b_origin,
	FileContext::ID file_id
) {
	TypeInfo &a = type_pool_.at(a_id), &b = type_pool_.at(b_id);

	bool a_matches = a.kind() == kind, b_matches = b.kind() == kind;
	if (!a_matches && !b_matches) return false;  // if neither match, this case is not for us
	if (a_matches && b_matches) return true;     // if both match, this is a freebie
	// we add a diagnostic
	std::stringstream subtitle_stream {};
	subtitle_stream
		<< "expected both types to be "
		<< (a_matches ? get_type_name(a_id) : get_type_name(b_id))
		<< "; got "
		<< (a_matches ? get_type_name(b_id) : get_type_name(a_id));
	parsed_files.at(file_id).diagnostics.push_back(
		Diagnostic::error(
			"type mismatch",
			subtitle_stream.str(),
			{get_type_sample(a_matches ? a_origin : b_origin, OutFmt::Color::Cyan),
	                 get_type_sample(a_matches ? b_origin : a_origin, OutFmt::Color::Yellow)}
		)
	);
	// whichever didn't match becomes a bottom
	if (!a_matches) a = TypeInfo::make_bottom();
	if (!b_matches) b = TypeInfo::make_bottom();
	return true;
}

void Resolver::unify_generics(
	TypeInfo::ID    generic,
	TypeInfo::ID    other,
	TypeInfo::ID    generic_origin,
	TypeInfo::ID    other_origin,
	FileContext::ID file_id
) {
	// this is an infallible operation
	assert(type_pool_.at(generic).is_generic());
	auto& a = type_pool_.at(generic).get_generic();

	// if the other type isn't a generic, this adds new imposed constraints to the generic
	if (!type_pool_.at(other).is_generic()) {
		a.imposed_constraints.push_back(TypeInfo::Generic::TypeConstraint {other_origin});
		return;
	}

	// if it is also a generic, we push each generic's declared constraints as imposed constraints for the other
	// generic
	auto& b = type_pool_.at(other).get_generic();

	// TODO: dedup
	for (auto const& constraint : a.declared_constraints) { b.imposed_constraints.push_back(constraint); }

	for (auto const& constraint : b.declared_constraints) { a.imposed_constraints.push_back(constraint); }
}

void Resolver::unify_member_access(
	TypeInfo::ID    member_access,
	TypeInfo::ID    other,
	TypeInfo::ID    member_access_origin,
	TypeInfo::ID    other_origin,
	FileContext::ID file_id
) {
	// for member access, the diagnostics will be thrown later anyways, so we always accept whatever happens here.
	assert(type_pool_.at(member_access).is_member_access());
	auto& a = type_pool_.at(member_access).get_member_access();

	// we push origins for diagnostics' sake
	if (!type_pool_.at(other).is_member_access()) {
		if (std::find(a.possible_types.cbegin(), a.possible_types.cend(), other_origin)
		    == a.possible_types.cend()) {
			a.possible_types.push_back(other_origin);
		}
		return;
	}

	auto& b = type_pool_.at(other).get_member_access();

	// make a list of all common possible types
	std::vector<TypeInfo::ID> new_possible_types {};
	new_possible_types.reserve(a.possible_types.size());
	std::copy(a.possible_types.cbegin(), a.possible_types.cend(), std::back_inserter(new_possible_types));
	std::copy_if(
		b.possible_types.cbegin(),
		b.possible_types.cend(),
		std::back_inserter(new_possible_types),
		[&new_possible_types](TypeInfo::ID type) {
			return std::find(new_possible_types.cbegin(), new_possible_types.cend(), type)
		            == new_possible_types.cend();
		}
	);

	// then set it for both!
	a.possible_types = new_possible_types;
	b.possible_types = std::move(new_possible_types);
}

void Resolver::unify_functions(
	TypeInfo::ID    function,
	TypeInfo::ID    other,
	TypeInfo::ID    function_origin,
	TypeInfo::ID    other_origin,
	FileContext::ID file_id
) {
	// ensure they're both functions
	assert(type_pool_.at(function).is_function());
	if (!type_pool_.at(other).is_function()) {
		std::stringstream subtitle_stream {};
		subtitle_stream << "expected both types to be functions; got " << get_type_name(other);
		parsed_files.at(file_id).diagnostics.push_back(
			Diagnostic::error(
				"type mismatch",
				subtitle_stream.str(),
				{get_type_sample(function_origin, OutFmt::Color::Cyan),
		                 get_type_sample(other_origin, OutFmt::Color::Yellow)}
			)
		);
		return;
	}

	TypeInfo::Function &a_function = type_pool_.at(function).get_function(),
			   &b_function = type_pool_.at(other).get_function();

	// ensure they have the same argument count
	if (a_function.arguments.size() != b_function.arguments.size()) {
		parsed_files.at(file_id).diagnostics.push_back(
			Diagnostic::error(
				"type mismatch",
				"expected both functions to have the same amount of arguments",
				{get_type_sample(function_origin, OutFmt::Color::Cyan),
		                 get_type_sample(other_origin, OutFmt::Color::Yellow)}
			)
		);
		return;
	}

	// unify the names and types of arguments
	for (size_t i = 0; i < a_function.arguments.size(); ++i) {
		auto& [a_name, a_type] = a_function.arguments.at(i);
		auto& [b_name, b_type] = b_function.arguments.at(i);

		// unify the type first for diagnostics' sake
		bool can_unify_type = can_unify(a_type, b_type);
		unify(a_type, b_type, file_id);

		bool a_has_name = a_name.has_value(), b_has_name = b_name.has_value();
		if (a_has_name && b_has_name && (a_name.value() != b_name.value())) {
			// we don't want to throw the diagnostic if the type cannot be unified to begin with
			if (can_unify_type) {
				// FIXME: we don't have the span/fileid for the argument, so we can't show a sample!
				parsed_files.at(file_id).diagnostics.push_back(
					Diagnostic::error(
						"type mismatch",
						"expected both functions to have the same argument names, since neither argument name is marked as anonymous",
						{}
					)
				);
			}
		} else if (a_has_name || b_has_name) {
			if (a_has_name) b_name = a_name;
			if (b_has_name) a_name = b_name;
		}
	}

	// unify the names and types of generics
	for (size_t i = 0; i < a_function.generics.size(); ++i) {
		auto& [a_name, a_type] = a_function.generics.at(i);
		auto& [b_name, b_type] = b_function.generics.at(i);

		// unify the type first for diagnostics' sake
		bool can_unify_type = can_unify(a_type, b_type);
		unify(a_type, b_type, file_id);

		bool a_has_name = a_name.has_value(), b_has_name = b_name.has_value();
		if (a_has_name && b_has_name && (a_name.value() != b_name.value())) {
			// we don't want to throw the diagnostic if the type cannot be unified to begin with
			if (can_unify_type) {
				// FIXME: we don't have the span/fileid for the generic, so we can't show a sample!
				parsed_files.at(file_id).diagnostics.push_back(
					Diagnostic::error(
						"type mismatch",
						"expected both functions to have the same generic names, since neither generic name is marked as anonymous",
						{}
					)
				);
			}
		} else if (a_has_name || b_has_name) {
			if (a_has_name) b_name = a_name;
			if (b_has_name) a_name = b_name;
		}
	}

	// unify the return types
	unify(a_function.return_, b_function.return_, file_id);
	return;
}

void Resolver::unify_pointers(
	TypeInfo::ID    pointer,
	TypeInfo::ID    other,
	TypeInfo::ID    pointer_origin,
	TypeInfo::ID    other_origin,
	FileContext::ID file_id
) {
	assert(type_pool_.at(pointer).is_pointer());
	if (!type_pool_.at(other).is_pointer()) {
		std::stringstream subtitle_stream {};
		subtitle_stream << "expected both types to be pointers; got " << get_type_name(other);
		parsed_files.at(file_id).diagnostics.push_back(
			Diagnostic::error(
				"type mismatch",
				subtitle_stream.str(),
				{get_type_sample(pointer_origin, OutFmt::Color::Cyan),
		                 get_type_sample(other_origin, OutFmt::Color::Yellow)}
			)
		);
		return;
	}

	TypeInfo::Pointer const &a_pointer = type_pool_.at(pointer).get_pointer(),
				&b_pointer = type_pool_.at(other).get_pointer();

	// ensure they point to the same type
	unify(a_pointer.pointee, b_pointer.pointee, pointer_origin, other_origin, file_id);

	// ensure they are of the same mutability
	if (a_pointer.mutable_ != b_pointer.mutable_) {
		parsed_files.at(file_id).diagnostics.push_back(
			Diagnostic::error(
				"type mismatch",
				"expected both pointers to have the same mutability",
				{get_type_sample(pointer_origin, OutFmt::Color::Cyan),
		                 get_type_sample(other_origin, OutFmt::Color::Yellow)}
			)
		);
		return;
	}
}

void Resolver::unify_named(
	TypeInfo::ID    named,
	TypeInfo::ID    other,
	TypeInfo::ID    named_origin,
	TypeInfo::ID    other_origin,
	FileContext::ID file_id
) {
	assert(type_pool_.at(named).is_named());
	if (!type_pool_.at(other).is_named()) {
		std::stringstream subtitle_stream {};
		subtitle_stream
			<< "expected both types to be "
			<< get_type_name(named)
			<< "; got "
			<< get_type_name(other);
		parsed_files.at(file_id).diagnostics.push_back(
			Diagnostic::error(
				"type mismatch",
				subtitle_stream.str(),
				{get_type_sample(named_origin, OutFmt::Color::Cyan),
		                 get_type_sample(other_origin, OutFmt::Color::Yellow)}
			)
		);
		return;
	}

	auto &a = type_pool_.at(named).get_named(), &b = type_pool_.at(other).get_named();

	// we must create a new candidate vector with only the common unified candidates
	std::vector<TypeInfo::Named::Candidate> common_candidates {};
	for (TypeInfo::Named::Candidate& candidate : a.candidates()) {
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
		for (size_t i = 0; i < candidate.generics.size(); ++i) {
			unify(candidate.generics.at(i), corresponding_candidate->generics.at(i), file_id);
		}
		common_candidates.push_back(std::move(candidate));
	}

	// if there are no common possibilities, this failed
	if (common_candidates.empty()) {
		std::stringstream subtitle_stream {};
		subtitle_stream
			<< "types "
			<< get_type_name(named)
			<< " and "
			<< get_type_name(other)
			<< " are incompatible";
		parsed_files.at(file_id).diagnostics.push_back(
			Diagnostic::error(
				"type mismatch",
				subtitle_stream.str(),
				{get_type_sample(named_origin, OutFmt::Color::Cyan),
		                 get_type_sample(other_origin, OutFmt::Color::Yellow)}
			)
		);
		return;
	}

	// otherwise, we can proceed with unification
	// FIXME: we need to bind this to both identifiers!!!
	TypeInfo common_type = TypeInfo::make_named(a.name, std::move(common_candidates));
	type_pool_.at(named) = common_type;
	type_pool_.at(other) = TypeInfo::make_same_as(named);
}

void Resolver::unify(
	TypeInfo::ID    a_id,
	TypeInfo::ID    b_id,
	TypeInfo::ID    a_origin,
	TypeInfo::ID    b_origin,
	FileContext::ID file_id
) {
	// safeguard
	if (a_id == b_id) return;

	TypeInfo &a = type_pool_.at(a_id), &b = type_pool_.at(b_id);

	// follow references
	if (a.is_same_as()) {
		a = unify_follow_references(a_id, b_id, a_origin, b_origin, file_id);
		return;
	}
	if (b.is_same_as()) {
		b = unify_follow_references(b_id, a_id, b_origin, a_origin, file_id);
		return;
	}

	// bottoms don't participate in unification
	if (a.is_bottom() || b.is_bottom()) return;

	// make unknowns known
	if (a.is_unknown()) {
		set_same_as(a_id, b_id);
		return;
	}
	if (b.is_unknown()) {
		set_same_as(b_id, a_id);
		return;
	}

	// generics
	if (a.is_generic()) return unify_generics(a_id, b_id, a_origin, b_origin, file_id);
	if (b.is_generic()) return unify_generics(b_id, a_id, b_origin, a_origin, file_id);

	// member access types
	if (a.is_member_access()) return unify_member_access(a_id, b_id, a_origin, b_origin, file_id);
	if (b.is_member_access()) return unify_member_access(b_id, a_id, b_origin, a_origin, file_id);

	// named types
	if (a.is_named()) return unify_named(a_id, b_id, a_origin, b_origin, file_id);
	if (b.is_named()) return unify_named(b_id, a_id, b_origin, a_origin, file_id);

	// if any of them is a basic Known type, the other must be exactly the same
	if (unify_basic_known(TypeInfo::Kind::KnownVoid, a_id, b_id, a_origin, b_origin, file_id)) return;
	if (unify_basic_known(TypeInfo::Kind::KnownChar, a_id, b_id, a_origin, b_origin, file_id)) return;
	if (unify_basic_known(TypeInfo::Kind::KnownBool, a_id, b_id, a_origin, b_origin, file_id)) return;

	// modules act like basic Known types right now, but this is silly.
	// TODO: do something better
	if (unify_basic_known(TypeInfo::Kind::Module, a_id, b_id, a_origin, b_origin, file_id)) return;

	// functions
	if (a.is_function()) return unify_functions(a_id, b_id, a_origin, b_origin, file_id);
	if (b.is_function()) return unify_functions(b_id, a_id, b_origin, a_origin, file_id);

	// pointers
	if (a.is_pointer()) return unify_pointers(a_id, b_id, a_origin, b_origin, file_id);
	if (b.is_pointer()) return unify_pointers(b_id, a_id, b_origin, a_origin, file_id);

	// now only numeric types are left ([Known/Partial][Integer/Float])
	bool a_known = a.is_known_integer() || a.is_known_float(), b_known = b.is_known_integer() || b.is_known_float();
	bool a_float = a.is_known_float() || a.is_partial_float(), b_float = b.is_known_float() || b.is_partial_float();

	// both must be either floats or integers
	if (a_float != b_float) {
		parsed_files.at(file_id).diagnostics.push_back(
			Diagnostic::error(
				"type mismatch",
				"incompatible numeric types: either the float must be truncated to an integer or the integer must be cast to a float",
				{get_type_sample(a_origin, OutFmt::Color::Cyan),
		                 get_type_sample(b_origin, OutFmt::Color::Yellow)}
			)
		);
		return;
	}

	// floats
	if (a_float) {
		// if neither are known, there is no information to be learnt, since we can only learn the bit
		// width
		if (!a_known && !b_known) return;

		// if both are known, we must ensure they don't clash
		if (a_known && b_known) {
			if (a.get_known_float().width != b.get_known_float().width) {
				parsed_files.at(file_id).diagnostics.push_back(
					Diagnostic::error(
						"type mismatch",
						"incompatible numeric types: floats of incompatible size",
						{get_type_sample(a_origin, OutFmt::Color::Cyan),
				                 get_type_sample(b_origin, OutFmt::Color::Yellow)}
					)
				);
				return;
			}
		}

		// if one is known, we can make the other the same as the other
		if (a_known) set_same_as(b_id, a_id);
		if (b_known) set_same_as(a_id, b_id);
		return;
	}

	// integers may clash due to sign and size
	bool signed_clash = false, size_clash = false;
	if (a_known && b_known) {
		// if both are known, we can just check clashes directly
		if (a.get_known_integer().integer.is_signed() != b.get_known_integer().integer.is_signed())
			signed_clash = true;
		if (a.get_known_integer().integer.width_type() != b.get_known_integer().integer.width_type())
			size_clash = true;
		if (a.get_known_integer().integer.bit_width() != b.get_known_integer().integer.bit_width())
			size_clash = true;
	} else if (a_known != b_known) {
		// if one isn't known, we must check that there are no clashes
		TypeInfo::PartialInteger const& partial = a_known ? b.get_partial_integer() : a.get_partial_integer();
		TypeInfo::KnownInteger const&   known   = a_known ? a.get_known_integer() : b.get_known_integer();

		// if the sign is known, it must not clash
		if (partial.signed_is_known && partial.integer.is_signed() != known.integer.is_signed())
			signed_clash = true;

		// if the size is known, it must not clash
		if (partial.integer.width_type() != AST::Type::Atom::Integer::WidthType::Any) {
			// the width types must match
			if (partial.integer.width_type() != known.integer.width_type()) size_clash = true;
			// if both are fixed width, their widths must match
			else if (partial.integer.bit_width() != known.integer.bit_width()) size_clash = true;
		}

		// if no clashes were detected, we unify by setting the partial one to the known one
		if (!signed_clash && !size_clash) { set_same_as(a_known ? b_id : a_id, a_known ? a_id : b_id); }
	} else {
		// neither are known, so we will construct a unified partial integer for both of them
		TypeInfo::PartialInteger const &a_partial = a.get_partial_integer(),
					       &b_partial = b.get_partial_integer();

		// let's determine the sign
		bool signed_ = false, signed_is_known;
		if (a_partial.signed_is_known && b_partial.signed_is_known) {
			// if the sign is known for both, it must not clash
			if (a_partial.integer.is_signed() != b_partial.integer.is_signed()) signed_clash = true;
			else {
				signed_is_known = true;
				signed_         = a_partial.integer.is_signed();
			}
		} else if (a_partial.signed_is_known != b_partial.signed_is_known) {
			// if the sign is known for one of them, we infer it from there
			signed_is_known = true;
			signed_         = a_partial.signed_is_known ? a_partial.integer.is_signed()
			                                            : b_partial.integer.is_signed();
		} else {
			// if it isn't known for either, we don't know anything
			signed_is_known = false;
		}

		// let's determine the width
		uint32_t width      = 0;
		auto     width_type = AST::Type::Atom::Integer::WidthType::Any;

		bool a_know_width = a_partial.integer.width_type() != AST::Type::Atom::Integer::WidthType::Any,
		     b_know_width = b_partial.integer.width_type() != AST::Type::Atom::Integer::WidthType::Any;
		if (a_know_width && b_know_width) {
			// if the width is known for both, it must not clash
			if (a_partial.integer.width_type() != b_partial.integer.width_type()) size_clash = true;
			else if (a_partial.integer.bit_width() != b_partial.integer.bit_width()) size_clash = true;
			else {
				width_type = a_partial.integer.width_type();
				if (width_type == AST::Type::Atom::Integer::WidthType::Fixed)
					width = a_partial.integer.bit_width().value();
			}
		} else if (a_know_width != b_know_width) {
			// if the width is known for only one of them, we infer it from there
			width_type = a_know_width ? a_partial.integer.width_type() : b_partial.integer.width_type();
			if (width_type == AST::Type::Atom::Integer::WidthType::Fixed)
				width = a_know_width ? a_partial.integer.bit_width().value()
				                     : b_partial.integer.bit_width().value();
		}

		if (!signed_clash && !size_clash) {
			// create the unified integer type
			AST::Type::Atom::Integer integer
				= width_type == AST::Type::Atom::Integer::WidthType::Any
			                ? AST::Type::Atom::Integer::any(signed_)
			        : width_type == AST::Type::Atom::Integer::WidthType::Size
			                ? AST::Type::Atom::Integer::size(signed_)
			        : width_type == AST::Type::Atom::Integer::WidthType::Ptr
			                ? AST::Type::Atom::Integer::ptr(signed_)
			                : AST::Type::Atom::Integer::with_width(width, signed_).value();

			TypeInfo new_type;
			if (signed_is_known && width_type != AST::Type::Atom::Integer::WidthType::Any) {
				// we know everything
				new_type = TypeInfo::make_known_integer(TypeInfo::KnownInteger {integer});
			} else {
				// we don't know everything, so this must be partial
				new_type = TypeInfo::make_partial_integer(
					TypeInfo::PartialInteger {integer, signed_is_known}
				);
			}

			// this is arbitrary, it works both ways
			b = new_type;
			a = TypeInfo::make_same_as(b_id);
		}
	}

	if (signed_clash)
		parsed_files.at(file_id).diagnostics.push_back(
			Diagnostic::error(
				"type mismatch",
				"incompatible numeric types: integers of incompatible sign",
				{get_type_sample(a_origin, OutFmt::Color::Cyan),
		                 get_type_sample(b_origin, OutFmt::Color::Yellow)}
			)
		);
	if (size_clash)
		parsed_files.at(file_id).diagnostics.push_back(
			Diagnostic::error(
				"type mismatch",
				"incompatible numeric types: integers of incompatible size",
				{get_type_sample(a_origin, OutFmt::Color::Cyan),
		                 get_type_sample(b_origin, OutFmt::Color::Yellow)}
			)
		);
}

void Resolver::unify(TypeInfo::ID a_id, TypeInfo::ID b_id, FileContext::ID file_id) {
	return unify(a_id, b_id, a_id, b_id, file_id);
}
