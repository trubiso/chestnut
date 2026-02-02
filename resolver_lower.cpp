#include "resolver.hpp"

#include <cstdlib>
#include <iostream>
#include <sstream>
#include <variant>

uint32_t const                     DEFAULT_INTEGER_WIDTH = 32;
IR::Type::Atom::Float::Width const DEFAULT_FLOAT_WIDTH   = IR::Type::Atom::Float::Width::F32;

IR::Type Resolver::reconstruct_type(TypeInfo::ID type_id, TypeInfo::ID type_origin) {
	TypeInfo const& type = type_pool_.at(type_id);
	switch (type.kind()) {
	// bottoms directly correspond to overconstrained types
	case TypeInfo::Kind::Bottom:    return IR::Type::make_atom(IR::Type::Atom::make_error());
	case TypeInfo::Kind::KnownVoid: return IR::Type::make_atom(IR::Type::Atom::make_void());
	case TypeInfo::Kind::KnownChar: return IR::Type::make_atom(IR::Type::Atom::make_char());
	case TypeInfo::Kind::KnownBool: return IR::Type::make_atom(IR::Type::Atom::make_bool());
	case TypeInfo::Kind::KnownFloat:
		return IR::Type::make_atom(
			IR::Type::Atom::make_float((IR::Type::Atom::Float::Width) type.get_known_float().width)
		);
	case TypeInfo::Kind::PartialFloat:   return IR::Type::make_atom(IR::Type::Atom::make_float(DEFAULT_FLOAT_WIDTH));
	case TypeInfo::Kind::Unknown:
	case TypeInfo::Kind::Module:
	case TypeInfo::Kind::Function:
	case TypeInfo::Kind::SameAs:
	case TypeInfo::Kind::KnownInteger:
	case TypeInfo::Kind::PartialInteger: break;
	}

	// FIXME: we throw a million diagnostics when there are resolved type cycles
	auto [span, file_id] = type_span_pool_.at(type_origin);
	if (type.kind() == TypeInfo::Kind::Unknown) {
		parsed_files.at(file_id).diagnostics.push_back(
			Diagnostic::error(
				"unknown type",
				"this type cannot be inferred automatically",
				{Diagnostic::Sample(get_context(file_id), span, OutFmt::Color::Red)}
			)
		);
		return IR::Type::make_atom(IR::Type::Atom::make_error());
	} else if (type.kind() == TypeInfo::Kind::Module) {
		parsed_files.at(file_id).diagnostics.push_back(
			Diagnostic::error(
				"invalid type",
				"modules cannot be used as values",
				{Diagnostic::Sample(get_context(file_id), span, OutFmt::Color::Red)}
			)
		);
		return IR::Type::make_atom(IR::Type::Atom::make_error());
	} else if (type.kind() == TypeInfo::Kind::Function) {
		// FIXME: this still does not have the correct span
		parsed_files.at(file_id).diagnostics.push_back(
			Diagnostic::error(
				"invalid type",
				"functions cannot be used as values",
				{Diagnostic::Sample(get_context(file_id), span, OutFmt::Color::Red)}
			)
		);
		return IR::Type::make_atom(IR::Type::Atom::make_error());
	} else if (type.kind() == TypeInfo::Kind::SameAs) {
		TypeInfo::SameAs const& same_as = type.get_same_as();
		// we should never reach this case
		assert(!same_as.ids.empty());
		if (same_as.ids.size() == 1) return reconstruct_type(same_as.ids.at(0), type_origin);
		parsed_files.at(file_id).diagnostics.push_back(
			Diagnostic::error(
				"unknown type",
				"this type cannot be decided from its possibilities",
				{get_type_sample(type_origin, OutFmt::Color::Red)}
			)
		);
		return IR::Type::make_atom(IR::Type::Atom::make_error());
	} else if (type.kind() == TypeInfo::Kind::KnownInteger) {
		auto const& integer = type.get_known_integer().integer;
		switch (integer.width_type()) {
		case AST::Type::Atom::Integer::WidthType::Fixed:
			return IR::Type::make_atom(
				IR::Type::Atom::make_integer(
					IR::Type::Atom::Integer::with_width(
						integer.bit_width().value(),
						integer.is_signed()
					)
						.value()
				)
			);
		case AST::Type::Atom::Integer::WidthType::Ptr:
			return IR::Type::make_atom(
				IR::Type::Atom::make_integer(IR::Type::Atom::Integer::ptr(integer.is_signed()))
			);
		case AST::Type::Atom::Integer::WidthType::Size:
			return IR::Type::make_atom(
				IR::Type::Atom::make_integer(IR::Type::Atom::Integer::size(integer.is_signed()))
			);
		case AST::Type::Atom::Integer::WidthType::Any: [[assume(false)]]; break;
		}
	} else if (type.kind() == TypeInfo::Kind::PartialInteger) {
		bool        signed_ = false;
		auto const& integer = type.get_partial_integer().integer;
		if (type.get_partial_integer().signed_is_known) signed_ = integer.is_signed();
		switch (integer.width_type()) {
		case AST::Type::Atom::Integer::WidthType::Any:
		case AST::Type::Atom::Integer::WidthType::Fixed:
			return IR::Type::make_atom(
				IR::Type::Atom::make_integer(
					IR::Type::Atom::Integer::with_width(
						integer.width_type() == AST::Type::Atom::Integer::WidthType::Any
							? DEFAULT_INTEGER_WIDTH
							: integer.bit_width().value(),
						signed_
					)
						.value()
				)
			);
		case AST::Type::Atom::Integer::WidthType::Ptr:
			return IR::Type::make_atom(IR::Type::Atom::make_integer(IR::Type::Atom::Integer::ptr(signed_)));
		case AST::Type::Atom::Integer::WidthType::Size:
			return IR::Type::make_atom(
				IR::Type::Atom::make_integer(IR::Type::Atom::Integer::size(signed_))
			);
		}
	}
}

IR::Type Resolver::reconstruct_type(TypeInfo::ID type_id) {
	return reconstruct_type(type_id, type_id);
}

Spanned<IR::Type> Resolver::lower_type(Spanned<AST::Type> spanned_type, FileContext::ID file_id) {
	auto [span, type] = std::move(spanned_type);
	if (type.kind() != AST::Type::Kind::Atom) {
		std::cout << "unsupported type non-atom" << std::endl;
		std::exit(0);
	}
	auto atom = type.get_atom();
	switch (atom.kind()) {
	case AST::Type::Atom::Kind::Float:
		return {span,
		        IR::Type::make_atom(
				IR::Type::Atom::make_float((IR::Type::Atom::Float::Width) atom.get_float().width)
			)};
	case AST::Type::Atom::Kind::Void:     return {span, IR::Type::make_atom(IR::Type::Atom::make_void())};
	case AST::Type::Atom::Kind::Char:     return {span, IR::Type::make_atom(IR::Type::Atom::make_char())};
	case AST::Type::Atom::Kind::Bool:     return {span, IR::Type::make_atom(IR::Type::Atom::make_bool())};
	case AST::Type::Atom::Kind::Integer:
	case AST::Type::Atom::Kind::Inferred: break;
	}

	if (atom.kind() == AST::Type::Atom::Kind::Integer) {
		auto const& integer = atom.get_integer();
		switch (integer.width_type()) {
		case AST::Type::Atom::Integer::WidthType::Any:
		case AST::Type::Atom::Integer::WidthType::Fixed:
			return {span,
			        IR::Type::make_atom(
					IR::Type::Atom::make_integer(
						IR::Type::Atom::Integer::with_width(
							integer.width_type() == AST::Type::Atom::Integer::WidthType::Any
								? DEFAULT_INTEGER_WIDTH
								: integer.bit_width().value(),
							integer.is_signed()
						)
							.value()
					)
				)};
		case AST::Type::Atom::Integer::WidthType::Ptr:
			return {span,
			        IR::Type::make_atom(
					IR::Type::Atom::make_integer(IR::Type::Atom::Integer::ptr(integer.is_signed()))
				)};
		case AST::Type::Atom::Integer::WidthType::Size:
			return {span,
			        IR::Type::make_atom(
					IR::Type::Atom::make_integer(IR::Type::Atom::Integer::size(integer.is_signed()))
				)};
		}
	} else if (atom.kind() == AST::Type::Atom::Kind::Inferred) {
		// this is invalid!! top level types must not be inferred
		parsed_files.at(file_id).diagnostics.push_back(
			Diagnostic::error(
				"type may not be inferred",
				"module item types must be fully specified",
				{Diagnostic::Sample(get_context(file_id), span, OutFmt::Color::Red)}
			)
		);
		return {span, IR::Type::make_atom(IR::Type::Atom::make_error())};
	}
}

std::tuple<Spanned<IR::Identifier>, IR::Type> Resolver::lower(Spanned<AST::Identifier> const& identifier) {
	auto [id, type] = lower(identifier.value);
	return {
		{identifier.span, id},
		type
	};
}

std::tuple<IR::Identifier, IR::Type> Resolver::lower(AST::Identifier const& identifier) {
	// for identifiers, we need to ensure that they are fully resolved
	if (!identifier.id.has_value() || identifier.id.value().size() != 1) {
		// we have no real way of skipping as of right now unfortunately
		// FIXME: this is dumb and silly and stupid and so on
		return {0, IR::Type::make_atom(IR::Type::Atom::make_error())};
	}
	AST::SymbolID id = identifier.id.value()[0];
	return {id, reconstruct_type(symbol_pool_.at(id).type)};
}

Spanned<IR::Identifier> Resolver::lower_identifier(Spanned<AST::Identifier> const& identifier) {
	return {identifier.span, identifier.value.id.value()[0]};
}

Spanned<IR::Expression::Atom>
Resolver::extract_expression(AST::Expression const& expression, Span span, IR::Scope& scope, FileContext::ID file_id) {
	AST::SymbolID id = next();
	symbol_pool_.push_back(Symbol {id, file_id, span, "_", {}, expression.type.value(), false, false, {}});
	Spanned<IR::Identifier> name {span, id};
	scope.push_back(
		Spanned<IR::Statement> {
			span,
			IR::Statement::make_declare(
				IR::Statement::Declare {
							name, reconstruct_type(expression.type.value()),
							lower(expression, span, scope, file_id),
							Spanned<bool> {span, false}
				}
			)
        }
	);
	return {name.span, IR::Expression::Atom::make_identifier(std::move(name.value))};
}

Spanned<IR::Expression::Atom>
Resolver::extract_expression(Spanned<AST::Expression> const& expression, IR::Scope& scope, FileContext::ID file_id) {
	return extract_expression(expression.value, expression.span, scope, file_id);
}

Spanned<IR::Expression>
Resolver::lower(AST::Expression::Atom const& atom, Span span, IR::Scope& scope, FileContext::ID file_id) {
	switch (atom.kind()) {
	case AST::Expression::Atom::Kind::NumberLiteral:
		return {span,
		        IR::Expression::make_atom(
				IR::Expression::Atom::make_literal(
					IR::Expression::Atom::Literal::Kind::Number,
					atom.get_number_literal().literal
				)
			)};
	case AST::Expression::Atom::Kind::StringLiteral:
		return {span,
		        IR::Expression::make_atom(
				IR::Expression::Atom::make_literal(
					IR::Expression::Atom::Literal::Kind::String,
					atom.get_string_literal().literal
				)
			)};
	case AST::Expression::Atom::Kind::CharLiteral:
		return {span,
		        IR::Expression::make_atom(
				IR::Expression::Atom::make_literal(
					IR::Expression::Atom::Literal::Kind::Char,
					atom.get_char_literal().literal
				)
			)};
	case AST::Expression::Atom::Kind::Expression:
		return {span,
		        IR::Expression::make_atom(
				extract_expression(*atom.get_expression(), span, scope, file_id).value
			)};
	case AST::Expression::Atom::Kind::Identifier:
		return {span,
		        IR::Expression::make_atom(
				IR::Expression::Atom::make_identifier(std::get<0>(lower(atom.get_identifier())))
			)};
	}
}

Spanned<IR::Expression> Resolver::lower(
	AST::Expression::UnaryOperation const& unary_operation,
	Span                                   span,
	IR::Scope&                             scope,
	FileContext::ID                        file_id
) {
	// TODO: operators
	Spanned<IR::Identifier> operator_ {span, 0};

	auto operand = extract_expression(*unary_operation.operand, scope, file_id);
	return Spanned<IR::Expression> {span, IR::Expression::make_function_call(std::move(operator_), {operand})};
}

Spanned<IR::Expression> Resolver::lower(
	AST::Expression::BinaryOperation const& binary_operation,
	Span                                    span,
	IR::Scope&                              scope,
	FileContext::ID                         file_id
) {
	// TODO: operators
	Spanned<IR::Identifier> operator_ {span, 0};

	auto lhs = extract_expression(*binary_operation.lhs, scope, file_id),
	     rhs = extract_expression(*binary_operation.rhs, scope, file_id);
	return Spanned<IR::Expression> {span, IR::Expression::make_function_call(std::move(operator_), {lhs, rhs})};
}

Spanned<IR::Expression> Resolver::lower(
	AST::Expression::FunctionCall const& function_call,
	Span                                 span,
	IR::Scope&                           scope,
	FileContext::ID                      file_id
) {
	// FIXME: better solution for fail expressions
	auto error_expression
		= Spanned<IR::Expression> {span, IR::Expression::make_atom(IR::Expression::Atom::make_identifier(0))};
	auto callee = lower(*function_call.callee, scope, file_id);
	// if the callee is not valid, we've already thrown diagnostics about it
	if (callee.value.kind() != IR::Expression::Kind::Atom
	    || callee.value.get_atom().kind() != IR::Expression::Atom::Kind::Identifier)
		return error_expression;
	Spanned<IR::Identifier> callee_identifier {callee.span, callee.value.get_atom().get_identifier()};

	// now we need to reconstruct the argument order from the type
	// these assertions shouldn't fail if we've successfully resolved, but you never know!
	TypeInfo::ID function_id = symbol_pool_.at(callee_identifier.value).type;
	if (type_pool_.at(function_id).kind() != TypeInfo::Kind::Function) return error_expression;
	TypeInfo::Function const& function = type_pool_.at(function_id).get_function();
	size_t argument_count = function_call.arguments.ordered.size() + function_call.arguments.labeled.size();
	assert(function.arguments.size() == argument_count);
	std::vector<Spanned<IR::Expression::Atom>> arguments {};
	arguments.reserve(argument_count);
	// ordered arguments are freebies
	for (AST::Expression::FunctionCall::OrderedArgument const& ordered_argument : function_call.arguments.ordered) {
		arguments.push_back(extract_expression(ordered_argument, scope, file_id));
	}
	// labeled arguments have to be reordered according to the function call type
	for (size_t i = function_call.arguments.ordered.size(); i < argument_count; ++i) {
		assert(std::get<0>(function.arguments.at(i)).has_value());
		std::string argument_name  = std::get<0>(function.arguments.at(i)).value();
		bool        argument_found = false;
		for (auto const& [label, argument] : function_call.arguments.labeled) {
			if (label.value.name() == argument_name) {
				arguments.push_back(extract_expression(argument, scope, file_id));
				argument_found = true;
			}
		}
		assert(argument_found);
	}
	assert(arguments.size() == argument_count);

	// finally, we have the callee and the arguments
	return {span, IR::Expression::make_function_call(std::move(callee_identifier), std::move(arguments))};
}

Spanned<IR::Expression>
Resolver::lower(AST::Expression const& expression, Span span, IR::Scope& scope, FileContext::ID file_id) {
	switch (expression.kind()) {
	case AST::Expression::Kind::Atom: return lower(expression.get_atom(), span, scope, file_id);
	case AST::Expression::Kind::UnaryOperation:
		return lower(expression.get_unary_operation(), span, scope, file_id);
	case AST::Expression::Kind::BinaryOperation:
		return lower(expression.get_binary_operation(), span, scope, file_id);
	case AST::Expression::Kind::FunctionCall: return lower(expression.get_function_call(), span, scope, file_id);
	}
}

Spanned<IR::Expression>
Resolver::lower(Spanned<AST::Expression> const& expression, IR::Scope& scope, FileContext::ID file_id) {
	return lower(expression.value, expression.span, scope, file_id);
}

std::optional<Spanned<IR::Statement>>
Resolver::lower(AST::Statement::Declare const& declare, Span span, IR::Scope& scope, FileContext::ID file_id) {
	auto identifier = lower(declare.name);

	auto value = declare.value.transform([&scope, file_id, this](auto&& value) {
		return lower(value, scope, file_id);
	});

	return Spanned<IR::Statement> {
		span,
		IR::Statement::make_declare(
			IR::Statement::Declare {
				std::get<0>(identifier),
				std::get<1>(identifier),
				value,
				declare.mutable_
			}
		)
	};
}

std::optional<Spanned<IR::Statement>>
Resolver::lower(AST::Statement::Set const& set, Span span, IR::Scope& scope, FileContext::ID file_id) {
	// skip invalid lhs
	if (!set.lhs.value.can_be_lhs()) return {};
	// TODO: in the future, once we do derefs, this will look different
	auto lhs            = lower(set.lhs, scope, file_id);
	auto lhs_identifier = Spanned<IR::Identifier> {lhs.span, lhs.value.get_atom().get_identifier()};
	// check that we're setting an actually mutable variable
	if (!symbol_pool_.at(lhs_identifier.value).mutable_) {
		Symbol const&     symbol = symbol_pool_.at(lhs_identifier.value);
		std::stringstream subtitle {};
		subtitle << "symbol '" << symbol.name << "' was not declared as mutable";
		parsed_files.at(file_id).diagnostics.push_back(
			Diagnostic::error(
				"tried to mutate immutable symbol",
				subtitle.str(),
				{Diagnostic::Sample(
					 get_context(symbol.file_id),
					 "declaration",
					 {Diagnostic::Sample::Label(symbol.span, OutFmt::Color::Cyan)}
				 ),
		                 Diagnostic::Sample(
					 get_context(file_id),
					 "mutation",
					 {Diagnostic::Sample::Label(lhs_identifier.span, OutFmt::Color::Red)}
				 )}
			)
		);
	}
	auto value = lower(set.rhs, scope, file_id);

	return Spanned<IR::Statement> {span, IR::Statement::make_set(IR::Statement::Set {lhs_identifier, value})};
}

std::optional<Spanned<IR::Statement>>
Resolver::lower(Spanned<AST::Statement> const& statement, IR::Scope& scope, FileContext::ID file_id) {
	switch (statement.value.kind()) {
	case AST::Statement::Kind::Declare: return lower(statement.value.get_declare(), statement.span, scope, file_id);
	case AST::Statement::Kind::Set:     return lower(statement.value.get_set(), statement.span, scope, file_id);
	case AST::Statement::Kind::Return:
		// we lower the expression if it exists, otherwise identical
		return Spanned<IR::Statement> {
			statement.span,
			IR::Statement::make_return(
				IR::Statement::Return {statement.value.get_return().value.transform(
					[&scope, file_id, this](auto&& value) { return lower(value, scope, file_id); }
				)}
			)
		};
	case AST::Statement::Kind::Scope:
		// we take advantage of the scope lowering function so it can do all the work for us
		for (Spanned<IR::Statement> const& ir_statement : lower(statement.value.get_scope(), file_id)) {
			scope.push_back(ir_statement);
		}

		return {};
	case AST::Statement::Kind::Expression: break;
	}

	// for expressions, it's a special case because we only care about function calls
	// however, when we resolve expressions, we do extract all potential inner function calls
	if (type_pool_.at(statement.value.get_expression().type.value()).kind() == TypeInfo::Kind::Bottom) return {};
	auto expression = lower(statement.value.get_expression(), statement.span, scope, file_id);
	if (expression.value.kind() != IR::Expression::Kind::FunctionCall) {
		parsed_files.at(file_id).diagnostics.push_back(
			Diagnostic::warning(
				"expression statement is not a function call",
				"if this expression contains any function calls, they will be evaluated, but it should be reduced to those function calls",
				{Diagnostic::Sample(
					get_context(file_id),
					{Diagnostic::Sample::Label(
						expression.span,
						"not a function call",
						OutFmt::Color::Yellow
					)}
				)}
			)
		);

		return {};
	}

	return Spanned<IR::Statement> {expression.span, {expression.value.get_function_call()}};
}

IR::Scope Resolver::lower(AST::Scope const& original_scope, FileContext::ID file_id) {
	IR::Scope scope {};
	for (Spanned<AST::Statement> const& statement : original_scope) {
		std::optional<Spanned<IR::Statement>> lowered_statement = lower(statement, scope, file_id);
		if (lowered_statement.has_value()) scope.push_back(lowered_statement.value());
	}
	// TODO: insert drop statements for all scope-local variables
	return scope;
}

IR::Function Resolver::lower(AST::Function const& function, FileContext::ID file_id) {
	std::vector<IR::Function::Argument> arguments {};
	arguments.reserve(function.arguments.size());
	for (auto const& [name, type, _, mutable_] : function.arguments) {
		// we don't need to push arguments as anonymous or mutable, we only cared during resolution and stuff
		arguments.push_back(IR::Function::Argument {lower_identifier(name), lower_type(type, file_id)});
	}
	auto body = function.body.transform([file_id, this](auto&& body) { return lower(body, file_id); });
	return IR::Function {
		{function.name.span, function.name.value.id.value()[0]},
		std::move(arguments),
		lower_type(function.return_type, file_id),
		body
	};
}

IR::Module Resolver::lower(AST::Module const& original_module, FileContext::ID file_id) {
	IR::Module module {lower_identifier(original_module.name), {}};
	// we don't need aliases anymore, those are purely for name resolution
	for (Spanned<AST::Module::Item> const& item : original_module.body.items) {
		auto& value = std::get<AST::Module::InnerItem>(item.value);
		if (std::holds_alternative<AST::Function>(value)) {
			AST::Function const& function = std::get<AST::Function>(value);
			lower(function, file_id);
			module.items.push_back(function.name.value.id.value()[0]);
		} else if (std::holds_alternative<AST::Module>(value)) {
			AST::Module const& submodule = std::get<AST::Module>(value);
			lower(submodule, file_id);
			module.items.push_back(submodule.name.value.id.value()[0]);
		}
	}
	return module;
}

std::vector<IR::Module> Resolver::lower() {
	std::vector<IR::Module> files {};
	files.reserve(parsed_files.size());
	for (ParsedFile const& file : parsed_files) { files.push_back(lower(file.module, file.file_id)); }
	return files;
}
