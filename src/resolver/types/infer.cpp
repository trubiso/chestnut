#include "resolver.hpp"

#include <algorithm>
#include <cstdlib>
#include <iostream>
#include <sstream>
#include <variant>

Resolver::TypeInfo::ID
Resolver::infer(AST::Expression::Atom::StructLiteral& struct_literal, Span span, FileContext::ID file_id) {
	TypeInfo named_type = from_type(struct_literal.type.value, file_id, false);
	if (named_type.is_bottom()) {
		// the function will have complained
		return register_type(TypeInfo::make_bottom(), span, file_id);
	}

	// we must now check field exhaustiveness to filter named types
	// TODO: store rejections
	std::vector<TypeInfo::Named::Candidate> suitable_candidates {};
	for (auto& candidate : named_type.get_named().candidates()) {
		AST::Struct* struct_ = std::get<AST::Struct*>(symbol_pool_.at(candidate.name).item);

		// first, we must check that all specified fields exist
		bool all_fields_exist = true;
		for (auto const& field : struct_literal.fields) {
			if (!std::any_of(
				    struct_->fields.cbegin(),
				    struct_->fields.cend(),
				    [&field](AST::Struct::Field const& struct_field) {
					    return field.name.value == struct_field.name.value;
				    }
			    )) {
				all_fields_exist = false;
				break;
			};
		}
		if (!all_fields_exist) continue;

		// then, we must check that all struct fields are specified
		bool all_fields_specified = true;
		for (auto const& struct_field : struct_->fields) {
			if (!std::any_of(
				    struct_literal.fields.cbegin(),
				    struct_literal.fields.cend(),
				    [&struct_field](AST::Expression::Atom::StructLiteral::Field const& field) {
					    return field.name.value == struct_field.name.value;
				    }
			    )) {
				all_fields_specified = false;
				break;
			};
		}
		if (!all_fields_specified) continue;

		// if both tests pass, this candidate is suitable
		suitable_candidates.push_back(std::move(candidate));
	}

	if (suitable_candidates.empty()) {
		// if there are no suitable candidates, we can't do anything
		parsed_files.at(file_id).diagnostics.push_back(
			Diagnostic::error(
				"struct literal does not match any struct",
				"none of the possible struct types have the same fields as the ones specified in the literal",
				{Diagnostic::Sample(get_context(file_id), span, OutFmt::Color::Red)}
			)
		);
		struct_literal.valid = false;  // remember to invalidate for lowering
		return register_type(TypeInfo::make_bottom(), span, file_id);
	}

	// remember to set the candidates to be the suitable ones
	named_type.get_named().candidates() = std::move(suitable_candidates);

	// we can get the field types via member access now and delegate the work there
	// TODO: make the span include the generic list
	TypeInfo::ID type_id = register_type(std::move(named_type), struct_literal.type.span, file_id);
	for (auto& field : struct_literal.fields) {
		TypeInfo::ID field_type  = infer(field.value->value, field.value->span, file_id);
		TypeInfo::ID access_type = register_type(
			TypeInfo::make_member_access(type_id, field.name.value),
			field.name.span,
			file_id
		);
		unify(field_type, access_type, file_id);
	}

	// finally, we return our named type
	return type_id;
}

Resolver::TypeInfo::ID Resolver::infer(AST::Expression::Atom& atom, Span span, FileContext::ID file_id) {
	switch (atom.kind()) {
	case AST::Expression::Atom::Kind::NumberLiteral:
		// TODO: apply suffixes
		if (atom.get_number_literal().is_float())
			return register_type(TypeInfo::make_partial_float(), span, file_id);
		else
			return register_type(
				TypeInfo::make_partial_integer(
					TypeInfo::PartialInteger {AST::Type::Atom::Integer::any(false), false}
				),
				span,
				file_id
			);
	case AST::Expression::Atom::Kind::StringLiteral:
		// TODO: do string literals
		std::cout << "unsupported string literal" << std::endl;
		std::exit(0);
	case AST::Expression::Atom::Kind::CharLiteral:
		// TODO: apply suffixes
		return register_type(TypeInfo::make_known_char(), span, file_id);
	case AST::Expression::Atom::Kind::BoolLiteral:   return register_type(TypeInfo::make_known_bool(), span, file_id);
	case AST::Expression::Atom::Kind::StructLiteral: return infer(atom.get_struct_literal(), span, file_id);
	case AST::Expression::Atom::Kind::Expression:    return infer(*atom.get_expression(), span, file_id);
	case AST::Expression::Atom::Kind::Identifier:    break;
	}

	// for identifiers, we match the type in the symbol pool
	AST::Identifier const& identifier = atom.get_identifier();
	if (!identifier.id.has_value())
		return register_type(
			TypeInfo::make_bottom(),
			span,
			file_id
		);  // if we don't know what this is, let's ignore
	std::vector<AST::SymbolID> const& ids = identifier.id.value();
	if (ids.empty()) return register_type(TypeInfo::make_bottom(), span, file_id);
	std::vector<TypeInfo::ID> type_ids {};
	type_ids.reserve(ids.size());
	std::transform(ids.cbegin(), ids.cend(), std::back_inserter(type_ids), [this](AST::SymbolID id) {
		return get_single_symbol(id).type;
	});
	if (type_ids.size() == 1) return register_type(TypeInfo::make_same_as(type_ids[0]), span, file_id);
	else return register_type(TypeInfo::make_same_as(std::move(type_ids)), span, file_id);
}

Resolver::TypeInfo::ID
Resolver::infer(AST::Expression::FunctionCall& function_call, Span span, FileContext::ID file_id) {
	// for function calls, we need to resolve or partially resolve the overload
	TypeInfo::ID callee_id = infer(function_call.callee->value, function_call.callee->span, file_id);

	// first, we ensure that there is at least one callable item
	if (!type_pool_.at(callee_id).is_callable(type_pool_)) {
		if (!type_pool_.at(callee_id).is_bottom())
			parsed_files.at(file_id).diagnostics.push_back(
				Diagnostic::error(
					"type mismatch",
					"attempted to call a non-function",
					{get_type_sample(callee_id, OutFmt::Color::Red)}
				)
			);
		return register_type(TypeInfo::make_bottom(), span, file_id);
	}

	// now, we flatten this type to a SameAs which points at all the callable types
	std::vector<TypeInfo::ID> callable = type_pool_.at(callee_id).get_callable_subitems(callee_id, type_pool_);
	assert(!callable.empty());  // we know is_callable()

	// we need to further filter this to ensure we have same argument count and our labeled arguments match
	// TODO: this works for now, but will break once arguments and generics have default values
	size_t provided_arguments = function_call.arguments.labeled.size() + function_call.arguments.ordered.size();
	size_t provided_generics  = function_call.generic_list.has_value()
	                                  ? (function_call.generic_list.value().labeled.size()
                                            + function_call.generic_list.value().ordered.size())
	                                  : 0;
	std::vector<TypeInfo::ID> callable_filtered {};
	// this list holds all function rejections as code samples so we can provide a rich diagnostic
	std::vector<Diagnostic::Sample> rejections {};
	// we do add the function call as a sample even though it is not a rejection
	rejections.push_back(
		Diagnostic::Sample(
			get_context(file_id),
			"call site",
			{Diagnostic::Sample::Label(span, "function call", OutFmt::Color::Gray)}
		)
	);
	// TODO: collapse these 2 for loops into one
	// TODO: generic count, generic labels
	for (TypeInfo::ID callable_id : callable) {
		assert(type_pool_.at(callable_id).is_function());
		auto const& function_arguments     = type_pool_.at(callable_id).get_function().arguments;
		bool        argument_count_matches = function_arguments.size() == provided_arguments;
		if (!argument_count_matches) {
			rejections.push_back(
				Diagnostic::Sample(
					get_context(get_type_file_id(callable_id)),
					{Diagnostic::Sample::Label(
						get_type_span(callable_id),
						function_arguments.size() < provided_arguments
							? "incompatible argument count (too many were provided)"
							: "incompatible argument count (too few were provided)",
						OutFmt::Color::BrightBlue
					)}
				)
			);
			continue;
		}
		if (!function_call.arguments.labeled.empty()) {
			// check that all labeled arguments exist
			// we only consider arguments that haven't already been provided by the ordered
			// arguments
			std::vector<std::string_view> arguments_under_consideration {};
			arguments_under_consideration.reserve(
				function_arguments.size() - function_call.arguments.ordered.size()
			);
			for (size_t i = function_call.arguments.ordered.size(); i < function_arguments.size(); ++i)
				if (std::get<0>(function_arguments.at(i)).has_value())
					arguments_under_consideration.push_back(
						std::get<0>(function_arguments.at(i)).value()
					);

			bool all_arguments_exist = true;
			for (auto const& argument : function_call.arguments.labeled) {
				auto const& identifier = std::get<0>(argument);

				bool argument_exists = std::any_of(
					arguments_under_consideration.cbegin(),
					arguments_under_consideration.cend(),
					[&identifier](std::string_view actual_argument) {
						return actual_argument == identifier.value.name();
					}
				);

				if (!argument_exists) {
					std::stringstream text {};
					text << "missing labeled argument `" << identifier.value.name() << "`";
					rejections.push_back(
						Diagnostic::Sample(
							get_context(get_type_file_id(callable_id)),
							{Diagnostic::Sample::Label(
								get_type_span(callable_id),
								text.str(),
								OutFmt::Color::BrightGreen
							)}
						)
					);
					all_arguments_exist = false;
					break;
				}
			}
			if (!all_arguments_exist) continue;
		}
		auto const& function_generics     = type_pool_.at(callable_id).get_function().generics;
		bool        generic_count_matches = function_generics.size() == provided_generics;
		if (!generic_count_matches) {
			rejections.push_back(
				Diagnostic::Sample(
					get_context(get_type_file_id(callable_id)),
					{Diagnostic::Sample::Label(
						get_type_span(callable_id),
						function_generics.size() < provided_generics
							? "incompatible generic count (too many were provided)"
							: "incompatible generic count (too few were provided)",
						OutFmt::Color::BrightBlue
					)}
				)
			);
			continue;
		}
		if (function_call.generic_list.has_value() && !function_call.generic_list.value().labeled.empty()) {
			// check that all labeled generics exist
			// we only consider generics that haven't already been provided by the ordered
			// generics
			std::vector<std::string_view> generics_under_consideration {};
			generics_under_consideration.reserve(
				function_generics.size() - function_call.generic_list.value().ordered.size()
			);
			for (size_t i = function_call.generic_list.value().ordered.size(); i < function_generics.size();
			     ++i)
				if (std::get<0>(function_generics.at(i)).has_value())
					generics_under_consideration.push_back(
						std::get<0>(function_generics.at(i)).value()
					);

			bool all_generics_exist = true;
			for (auto const& generic : function_call.generic_list.value().labeled) {
				auto const& name = std::get<0>(generic);

				bool generic_exists = std::any_of(
					generics_under_consideration.cbegin(),
					generics_under_consideration.cend(),
					[&name](std::string_view actual_generic) {
						return actual_generic == name.value;
					}
				);

				if (!generic_exists) {
					std::stringstream text {};
					text << "missing labeled generic `" << name.value << "`";
					rejections.push_back(
						Diagnostic::Sample(
							get_context(get_type_file_id(callable_id)),
							{Diagnostic::Sample::Label(
								get_type_span(callable_id),
								text.str(),
								OutFmt::Color::BrightGreen
							)}
						)
					);
					all_generics_exist = false;
					break;
				}
			}
			if (!all_generics_exist) continue;
		}
		callable_filtered.push_back(callable_id);
	}

	if (callable_filtered.empty()) {
		parsed_files.at(file_id).diagnostics.push_back(
			Diagnostic::error(
				"could not resolve function overload",
				"no function matched the constraints imposed by the function call",
				std::move(rejections)
			)
		);

		// we need to "unresolve" the callee identifier just in case
		if (function_call.callee->value.is_atom() || function_call.callee->value.get_atom().is_identifier())
			function_call.callee->value.get_atom().get_identifier().id = {};

		return register_type(TypeInfo::make_bottom(), span, file_id);
	}

	// it's time to infer the arguments now that we have a set of possible callees
	std::vector<TypeInfo::ID>                     ordered_arguments {};
	std::unordered_map<std::string, TypeInfo::ID> labeled_arguments {};
	ordered_arguments.reserve(function_call.arguments.ordered.size());
	std::transform(
		function_call.arguments.ordered.begin(),
		function_call.arguments.ordered.end(),
		std::back_inserter(ordered_arguments),
		[this, file_id](auto& argument) { return infer(argument.value, argument.span, file_id); }
	);
	ordered_arguments.reserve(function_call.arguments.labeled.size());
	for (auto& [identifier, value] : function_call.arguments.labeled) {
		labeled_arguments.emplace(identifier.value.name(), infer(value.value, value.span, file_id));
	}

	std::vector<TypeInfo::ID>                     ordered_generics {};
	std::unordered_map<std::string, TypeInfo::ID> labeled_generics {};
	if (function_call.generic_list.has_value()) {
		ordered_generics.reserve(function_call.generic_list.value().ordered.size());
		std::transform(
			function_call.generic_list.value().ordered.begin(),
			function_call.generic_list.value().ordered.end(),
			std::back_inserter(ordered_generics),
			[this, file_id](auto& generic) {
				return register_type(from_type(generic.value, file_id, false), generic.span, file_id);
			}
		);
		ordered_generics.reserve(function_call.generic_list.value().labeled.size());
		for (auto& [name, generic] : function_call.generic_list.value().labeled) {
			labeled_generics.emplace(
				name.value,
				register_type(from_type(generic.value, file_id, false), generic.span, file_id)
			);
		}
	}

	TypeInfo::ID expr_type = register_type(TypeInfo::make_unknown(), span, file_id);

	// let's store all of the candidates
	std::vector<UndecidedOverload::Candidate> candidates {};
	candidates.reserve(callable_filtered.size());
	for (TypeInfo::ID callable_id : callable_filtered) {
		assert(type_pool_.at(callable_id).is_function());
		// now we must create the function call type according to the function (due to labeled
		// arguments and generics)
		auto const& function_arguments = type_pool_.at(callable_id).get_function().arguments;
		assert(function_arguments.size() == provided_arguments);
		std::vector<std::tuple<std::optional<std::string>, TypeInfo::ID>> arguments {};
		arguments.reserve(provided_arguments);
		std::transform(
			ordered_arguments.cbegin(),
			ordered_arguments.cend(),
			std::back_inserter(arguments),
			[](TypeInfo::ID argument) { return std::tuple {std::nullopt, argument}; }
		);
		for (size_t i = arguments.size(); i < function_arguments.size(); ++i) {
			std::optional<std::string> name = std::get<0>(function_arguments.at(i));
			// if it didn't have a name, it couldn't be a labeled argument
			assert(name.has_value());
			arguments.push_back({name, labeled_arguments.at(name.value())});
		}

		auto const& function_generics = type_pool_.at(callable_id).get_function().generics;
		assert(function_generics.size() == provided_generics);
		std::vector<std::tuple<std::optional<std::string>, TypeInfo::ID>> generics {};
		generics.reserve(provided_generics);
		// FIXME: this breaks with built-in functions!
		auto const& original_function
			= *std::get<AST::Function*>(symbol_pool_.at(type_symbol_mapping_.at(callable_id).value()).item);
		for (size_t i = 0; i < provided_generics; ++i) {
			auto id = original_function.generic_declaration.value().generics.at(i).name.value.id.value().at(
				0
			);
			if (i < ordered_generics.size()) {
				generics.emplace_back(std::nullopt, generify_type(ordered_generics.at(i), id));
			} else {
				std::optional<std::string> name = std::get<0>(function_generics.at(i));
				// if it didn't have a name, it couldn't be a labeled generic
				assert(name.has_value());
				generics.emplace_back(name, generify_type(labeled_generics.at(name.value()), id));
			}
		}

		std::unordered_map<TypeInfo::ID, TypeInfo::ID> temp_map {};
		for (size_t i = 0; i < provided_generics; ++i) {
			temp_map.insert_or_assign(std::get<1>(function_generics.at(i)), std::get<1>(generics.at(i)));
		}

		TypeInfo::ID temp_type = instantiate_type(callable_id, temp_map);

		// we store the expression type as the return type so it automatically gets inferred!
		std::unordered_map<TypeInfo::ID, TypeInfo::ID> generic_map;
		TypeInfo::ID callable_type = instantiate_type(callable_id, generic_map);
		// let's replace the type spans so the diagnostics are a bit nicer :-)
		// TODO: this is a bit of a silly solution isn't it
		for (size_t i = 0; i < generics.size(); ++i) {
			type_span_pool_.at(std::get<1>(type_pool_.at(callable_type).get_function().generics.at(i)))
				= type_span_pool_.at(std::get<1>(generics.at(i)));
		}
		TypeInfo function_call_type = TypeInfo::make_function(
			TypeInfo::Function {std::move(arguments), std::move(generics), expr_type}
		);
		TypeInfo::ID call_id = register_type(std::move(function_call_type), span, file_id);
		unify(call_id, temp_type, file_id);
		UndecidedOverload::Candidate candidate {callable_type, call_id};
		candidates.push_back(std::move(candidate));
	}

	std::optional<AST::Identifier*> identifier = std::nullopt;

	if (function_call.callee->value.is_atom() || function_call.callee->value.get_atom().is_identifier())
		identifier = &function_call.callee->value.get_atom().get_identifier();

	UndecidedOverload overload {
		expr_type,
		identifier,
		std::move(candidates),
		std::move(rejections),
		span,
		file_id,
		&function_call
	};

	if (!try_decide(overload)) undecided_overloads.push_back(std::move(overload));

	return expr_type;
}

Resolver::TypeInfo::ID
Resolver::infer(AST::Expression::MemberAccess& member_access, Span span, FileContext::ID file_id) {
	TypeInfo::ID accessee_id = infer(member_access.accessee->value, member_access.accessee->span, file_id);

	TypeInfo     type = TypeInfo::make_member_access(accessee_id, member_access.field.value);
	TypeInfo::ID id   = register_type(std::move(type), span, file_id);

	if (!try_decide(id)) undecided_member_accesses.push_back(id);

	return id;
}

Resolver::TypeInfo::ID Resolver::infer(AST::Expression& expression, Span span, FileContext::ID file_id) {
	switch (expression.kind()) {
	case AST::Expression::Kind::Atom:             expression.type = infer(expression.get_atom(), span, file_id); break;
	case AST::Expression::Kind::UnaryOperation:
	case AST::Expression::Kind::AddressOperation:
	case AST::Expression::Kind::BinaryOperation:  break;
	case AST::Expression::Kind::FunctionCall:
		expression.type = infer(expression.get_function_call(), span, file_id);
		break;
	case AST::Expression::Kind::MemberAccess:
		expression.type = infer(expression.get_member_access(), span, file_id);
		break;
	case AST::Expression::Kind::If: [[assume(false)]]; return expression.type.value();
	}

	// for operations, we turn them into function calls and then resolve them
	if (expression.is_unary_operation() || expression.is_binary_operation()) {
		bool is_unary = expression.is_unary_operation();

		Token::Symbol operator_ = is_unary ? expression.get_unary_operation().operation
		                                   : expression.get_binary_operation().operation;

		if (is_unary && operator_ == Token::Symbol::Star) {
			// the dereference operator cannot be overloaded either
			TypeInfo::ID argument
				= infer(expression.get_unary_operation().operand->value,
			                expression.get_unary_operation().operand->span,
			                file_id);
			if (!type_pool_.at(argument).is_pointer(type_pool_)) {
				parsed_files.at(file_id).diagnostics.push_back(
					Diagnostic::error(
						"type mismatch",
						"only pointers can be dereferenced",
						{get_type_sample(argument, OutFmt::Color::Red)}
					)
				);
				expression.type = register_type(TypeInfo::make_bottom(), span, file_id);

				return expression.type.value();
			}

			expression.type = type_pool_.at(argument).get_pointee(type_pool_);
		} else {
			// TODO: get the operator span
			Span operator_span = span;

			AST::Identifier callee_identifier {
				{operator_span, get_variant_name(operator_)}
			};
			callee_identifier.id   = get_operator_candidates(operator_, !is_unary);
			AST::Expression callee = AST::Expression::make_atom(
				AST::Expression::Atom::make_identifier(std::move(callee_identifier))
			);

			AST::Expression::FunctionCall::Arguments arguments {{}, {}};
			if (is_unary) arguments.ordered.push_back(std::move(*expression.get_unary_operation().operand));
			else {
				arguments.ordered.push_back(std::move(*expression.get_binary_operation().lhs));
				arguments.ordered.push_back(std::move(*expression.get_binary_operation().rhs));
			}

			expression = AST::Expression::make_function_call(
				std::make_unique<Spanned<AST::Expression>>(
					Spanned<AST::Expression> {operator_span, std::move(callee)}
				),
				std::move(arguments)
			);
			// TODO: change the diagnostics here to refer to operators and not functions
			expression.type = infer(expression.get_function_call(), span, file_id);
		}
	}

	// for address operations, we infer the inner type and deduce the type from there (no overloads are supported
	// for these)
	if (expression.is_address_operation()) {
		TypeInfo::ID inner
			= infer(expression.get_address_operation().operand->value,
		                expression.get_address_operation().operand->span,
		                file_id);
		expression.type = register_type(
			TypeInfo::make_pointer(TypeInfo::Pointer {inner, expression.get_address_operation().mutable_}),
			span,
			file_id
		);
	}

	return expression.type.value();
}

void Resolver::infer(AST::Statement::Declare& declare, FileContext::ID file_id) {
	if (!declare.value.has_value()) return;
	TypeInfo::ID variable_type = get_single_symbol(declare.name.value).type;
	TypeInfo::ID value_type    = infer(declare.value.value().value, declare.value.value().span, file_id);
	// we must make sure that the declared and actual type match
	unify(variable_type, value_type, file_id);
}

void Resolver::infer(AST::Statement::Set& set, FileContext::ID file_id) {
	TypeInfo::ID rhs_type = infer(set.rhs.value, set.rhs.span, file_id);
	// skip all invalid LHS
	if (!set.lhs.value.can_be_lhs()) return;
	if (set.lhs.value.is_atom()) {
		// identifiers
		if (!set.lhs.value.get_atom().is_identifier()) return;
		// if name resolution failed, we must move on
		if (!set.lhs.value.get_atom().get_identifier().id.has_value()) return;
		if (set.lhs.value.get_atom().get_identifier().id.value().empty()) return;
		// TODO: remove this, because set statements either have one lhs or most likely they should be forbidden
		if (set.lhs.value.get_atom().get_identifier().id.value().size() > 1) {
			std::cout
				<< "we gotta resolve the lhs for some reason? check whether nameres threw a diagnostic"
				<< std::endl;
			return;
		}
		TypeInfo::ID lhs_type = infer(set.lhs.value, set.lhs.span, file_id);
		unify(lhs_type, rhs_type, file_id);
	} else if (set.lhs.value.is_unary_operation()) {
		// derefs
		if (set.lhs.value.get_unary_operation().operation != Token::Symbol::Star) return;
		TypeInfo::ID lhs_type = infer(set.lhs.value, set.lhs.span, file_id);
		if (!type_pool_.at(lhs_type).is_pointer()) return;
		TypeInfo::Pointer const& pointer = type_pool_.at(lhs_type).get_pointer();
		unify(pointer.pointee, rhs_type, lhs_type, rhs_type, file_id);
	} else if (set.lhs.value.is_member_access()) {
		// member access is as simple as identifier lhs
		TypeInfo::ID lhs_type = infer(set.lhs.value, set.lhs.span, file_id);
		unify(lhs_type, rhs_type, file_id);
	}
}

void Resolver::infer(AST::Statement::Return& return_, Span span, AST::SymbolID function, FileContext::ID file_id) {
	TypeInfo::ID return_value = return_.value.has_value()
	                                  // if we do have a value, get its expression type
	                                  ? infer(return_.value.value().value, return_.value.value().span, file_id)
	                                  // if we don't, that's a return void
	                                  : register_type(TypeInfo::make_known_void(), span, file_id);
	// we must make sure that the return type and the returned value match
	unify(return_value, type_pool_.at(symbol_pool_.at(function).type).get_function().return_, file_id);
}

void Resolver::infer(Spanned<AST::Statement>& statement, AST::SymbolID function, FileContext::ID file_id) {
	switch (statement.value.kind()) {
	case AST::Statement::Kind::Declare:    infer(statement.value.get_declare(), file_id); return;
	case AST::Statement::Kind::Set:        infer(statement.value.get_set(), file_id); return;
	case AST::Statement::Kind::Expression: break;
	case AST::Statement::Kind::Return:
		infer(statement.value.get_return(), statement.span, function, file_id);
		return;
	case AST::Statement::Kind::Scope: infer(statement.value.get_scope(), function, file_id); return;
	case AST::Statement::Kind::Label:
	case AST::Statement::Kind::Goto:  return;
	case AST::Statement::Kind::Branch:
		// we need to infer the condition's type and unify it with bool
		infer(statement.value.get_branch().condition.value,
		      statement.value.get_branch().condition.span,
		      file_id);
		// FIXME: we need a better solution for built-in bool requirement
		unify(statement.value.get_branch().condition.value.type.value(),
		      register_type(TypeInfo::make_known_bool(), statement.value.get_branch().condition.span, file_id),
		      file_id);
		return;
	case AST::Statement::Kind::If: return;
	}

	// for expression statements, we want to throw a warning if it results in a non-void result
	TypeInfo::ID type_id = infer(statement.value.get_expression(), statement.span, file_id);
	if (type_pool_.at(type_id).is_bottom()) return;  // skip bottoms
	if (!can_unify(type_id, TypeInfo::make_known_void())) {
		parsed_files.at(file_id).diagnostics.push_back(
			Diagnostic::warning(
				"discarded expression result",
				"this expression's result is non-void",
				{get_type_sample(type_id, OutFmt::Color::Yellow)}
			)
		);
	}
}

void Resolver::infer(AST::Scope& scope, AST::SymbolID function, FileContext::ID file_id) {
	for (auto& statement : scope) { infer(statement, function, file_id); }
}

void Resolver::infer(AST::GenericDeclaration& generic_declaration, FileContext::ID file_id) {
	for (auto& generic : generic_declaration.generics) ensure_has_constraints(generic, file_id);
}

void Resolver::infer(AST::Function& function, FileContext::ID file_id) {
	if (function.generic_declaration.has_value()) infer(function.generic_declaration.value(), file_id);
	if (function.body.has_value()) infer(function.body.value(), function.name.value.id.value()[0], file_id);
}

void Resolver::infer(AST::Struct& struct_, FileContext::ID file_id) {
	if (struct_.generic_declaration.has_value()) infer(struct_.generic_declaration.value(), file_id);
	for (auto& field : struct_.fields) {
		field.type_id = register_type(from_type(field.type.value, file_id, false), field.type.span, file_id);
	}
}

void Resolver::infer(AST::Trait& trait, FileContext::ID file_id) {
	if (trait.generic_declaration.has_value()) infer(trait.generic_declaration.value(), file_id);
	for (auto& constraint : trait.constraints) {
		// we generate the constraint and that does all of the generics for us :P
		auto temp_constraint = AST::GenericDeclaration::Generic::Constraint {
			constraint.name,
			std::move(constraint.generic_list)
		};
		auto trait_constraint = generate_constraint(temp_constraint, file_id);
		if (!trait_constraint.has_value()) continue;
		get_single_symbol(trait.name.value).trait_constraints.push_back(trait_constraint.value());
	}
}

void Resolver::infer(AST::Module& module, FileContext::ID file_id) {
	for (Spanned<AST::Module::Item>& item : module.body.items) {
		auto& value = std::get<AST::Module::InnerItem>(item.value);
		if (std::holds_alternative<AST::Function>(value)) infer(std::get<AST::Function>(value), file_id);
		else if (std::holds_alternative<AST::Module>(value)) infer(std::get<AST::Module>(value), file_id);
		else if (std::holds_alternative<AST::Struct>(value)) infer(std::get<AST::Struct>(value), file_id);
		else if (std::holds_alternative<AST::Trait>(value)) infer(std::get<AST::Trait>(value), file_id);
	}
}
