#include "resolver.hpp"

#include <algorithm>
#include <cstdlib>
#include <iostream>
#include <sstream>
#include <variant>

uint32_t const                     DEFAULT_INTEGER_WIDTH = 32;
IR::Type::Atom::Float::Width const DEFAULT_FLOAT_WIDTH   = IR::Type::Atom::Float::Width::F32;

IR::Type Resolver::reconstruct_type(TypeInfo::ID type_id, TypeInfo::ID type_origin, bool allow_functions) {
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
		if (!allow_functions)
			parsed_files.at(file_id).diagnostics.push_back(
				Diagnostic::error(
					"invalid type",
					"functions cannot be used as values",
					{Diagnostic::Sample(get_context(file_id), span, OutFmt::Color::Red)}
				)
			);
		// even if we allow functions, we don't have a type for them :P
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

IR::Type Resolver::reconstruct_type(TypeInfo::ID type_id, bool allow_functions) {
	return reconstruct_type(type_id, type_id, allow_functions);
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

std::optional<std::tuple<Spanned<IR::Identifier>, IR::Type>>
Resolver::lower(Spanned<AST::Identifier> const& identifier, bool allow_functions) {
	auto data = lower(identifier.value, allow_functions);
	if (!data.has_value()) return std::nullopt;
	auto [id, type] = std::move(data.value());
	Spanned<IR::Identifier> resolved_identifier {identifier.span, id};
	return std::tuple<Spanned<IR::Identifier>, IR::Type> {resolved_identifier, std::move(type)};
}

std::optional<std::tuple<IR::Identifier, IR::Type>>
Resolver::lower(AST::Identifier const& identifier, bool allow_functions) {
	// for identifiers, we need to ensure that they are fully resolved
	if (!identifier.id.has_value() || identifier.id.value().size() != 1) { return std::nullopt; }
	AST::SymbolID id = identifier.id.value()[0];
	return std::tuple {id, reconstruct_type(symbol_pool_.at(id).type, allow_functions)};
}

Spanned<IR::Identifier> Resolver::lower_identifier(Spanned<AST::Identifier> const& identifier) {
	return {identifier.span, identifier.value.id.value()[0]};
}

Spanned<IR::Expression::Atom> Resolver::extract_expression(
	AST::Expression const&       expression,
	Span                         span,
	std::vector<IR::BasicBlock>& basic_blocks,
	FileContext::ID              file_id,
	bool                         allow_functions
) {
	AST::SymbolID id = symbol_next();
	symbol_pool_.push_back(Symbol {id, file_id, span, "_", {}, expression.type.value(), false, false, {}});
	Spanned<IR::Identifier> name {span, id};

	IR::Type type = reconstruct_type(expression.type.value(), allow_functions);
	// we actually make the statement before pushing it just in case this lowering creates a new basic block
	Spanned<IR::Statement> statement {
		span,
		IR::Statement::make_declare(
			IR::Statement::Declare {
						name, type.clone(),
						lower(expression, span, basic_blocks, file_id),
						Spanned<bool> {span, false}
			}
		)
	};
	basic_blocks.at(basic_blocks.size() - 1).statements.push_back(std::move(statement));
	return {name.span, IR::Expression::Atom::make_identifier(std::move(name.value), std::move(type))};
}

Spanned<IR::Expression::Atom> Resolver::extract_expression(
	Spanned<AST::Expression> const& expression,
	std::vector<IR::BasicBlock>&    basic_blocks,
	FileContext::ID                 file_id,
	bool                            allow_functions
) {
	return extract_expression(expression.value, expression.span, basic_blocks, file_id, allow_functions);
}

Spanned<IR::Expression> Resolver::lower(
	AST::Expression::Atom const& atom,
	TypeInfo::ID                 type_id,
	Span                         span,
	std::vector<IR::BasicBlock>& basic_blocks,
	FileContext::ID              file_id,
	bool                         allow_functions
) {
	switch (atom.kind()) {
	case AST::Expression::Atom::Kind::NumberLiteral:
		return {span,
		        IR::Expression::make_atom(
				IR::Expression::Atom::make_literal(
					IR::Expression::Atom::Literal::Kind::Number,
					atom.get_number_literal().literal,
					reconstruct_type(type_id)
				)
			)};
	case AST::Expression::Atom::Kind::StringLiteral:
		return {span,
		        IR::Expression::make_atom(
				IR::Expression::Atom::make_literal(
					IR::Expression::Atom::Literal::Kind::String,
					atom.get_string_literal().literal,
					reconstruct_type(type_id)
				)
			)};
	case AST::Expression::Atom::Kind::CharLiteral:
		return {span,
		        IR::Expression::make_atom(
				IR::Expression::Atom::make_literal(
					IR::Expression::Atom::Literal::Kind::Char,
					atom.get_char_literal().literal,
					reconstruct_type(type_id)
				)
			)};
	case AST::Expression::Atom::Kind::BoolLiteral:
		return {span,
		        IR::Expression::make_atom(
				IR::Expression::Atom::make_bool(
					atom.get_bool_literal().value,
					reconstruct_type(type_id)
				)
			)};
	case AST::Expression::Atom::Kind::Expression:
		return {span,
		        IR::Expression::make_atom(
				extract_expression(*atom.get_expression(), span, basic_blocks, file_id).value
			)};
	case AST::Expression::Atom::Kind::Identifier: break;
	}
	// for identifiers, we need to extract the type as well.
	// only identifiers can be functions!
	auto data = lower(atom.get_identifier(), allow_functions);
	if (!data.has_value()) return {span, IR::Expression::make_atom(IR::Expression::Atom::make_error())};
	auto [identifier, type] = std::move(data.value());
	return {span, IR::Expression::make_atom(IR::Expression::Atom::make_identifier(identifier, std::move(type)))};
}

Spanned<IR::Expression> Resolver::lower(
	AST::Expression::FunctionCall const& function_call,
	Span                                 span,
	std::vector<IR::BasicBlock>&         basic_blocks,
	FileContext::ID                      file_id,
	bool                                 allow_functions
) {
	auto error_expression
		= Spanned<IR::Expression> {span, IR::Expression::make_atom(IR::Expression::Atom::make_error())};
	auto callee = lower(*function_call.callee, basic_blocks, file_id, true);
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
	std::transform(
		function_call.arguments.ordered.cbegin(),
		function_call.arguments.ordered.cend(),
		std::back_inserter(arguments),
		[this, &basic_blocks, file_id](auto const& ordered_argument) {
			return extract_expression(ordered_argument, basic_blocks, file_id);
		}
	);
	// labeled arguments have to be reordered according to the function call type
	for (size_t i = function_call.arguments.ordered.size(); i < argument_count; ++i) {
		assert(std::get<0>(function.arguments.at(i)).has_value());
		std::string argument_name  = std::get<0>(function.arguments.at(i)).value();
		bool        argument_found = false;
		for (auto const& [label, argument] : function_call.arguments.labeled) {
			if (label.value.name() == argument_name) {
				arguments.push_back(extract_expression(argument, basic_blocks, file_id));
				argument_found = true;
			}
		}
		assert(argument_found);
	}
	assert(arguments.size() == argument_count);

	// finally, we have the callee and the arguments
	// TODO: check if the result is a function for allow_functions
	return {span, IR::Expression::make_function_call(callee_identifier.value, std::move(arguments))};
}

Spanned<IR::Expression> Resolver::lower(
	AST::Expression const&       expression,
	Span                         span,
	std::vector<IR::BasicBlock>& basic_blocks,
	FileContext::ID              file_id,
	bool                         allow_functions
) {
	switch (expression.kind()) {
	case AST::Expression::Kind::Atom:
		return lower(
			expression.get_atom(),
			expression.type.value(),
			span,
			basic_blocks,
			file_id,
			allow_functions
		);
	case AST::Expression::Kind::FunctionCall:
		return lower(expression.get_function_call(), span, basic_blocks, file_id, allow_functions);
	case AST::Expression::Kind::UnaryOperation:
	case AST::Expression::Kind::BinaryOperation:
	case AST::Expression::Kind::If:              [[assume(false)]];
	}
}

Spanned<IR::Expression> Resolver::lower(
	Spanned<AST::Expression> const& expression,
	std::vector<IR::BasicBlock>&    basic_blocks,
	FileContext::ID                 file_id,
	bool                            allow_functions
) {
	return lower(expression.value, expression.span, basic_blocks, file_id, allow_functions);
}

std::optional<Spanned<IR::Statement>> Resolver::lower(
	AST::Statement::Declare const& declare,
	Span                           span,
	std::vector<IR::BasicBlock>&   basic_blocks,
	FileContext::ID                file_id
) {
	auto data = lower(declare.name);
	if (!data.has_value()) return {};
	auto [name, type] = std::move(data.value());

	auto value = declare.value.transform([&basic_blocks, file_id, this](auto&& value) {
		return lower(value, basic_blocks, file_id);
	});

	if (declare.is_undefined) assert(!value.has_value());

	// value will only be {} if it is undefined
	if (!declare.is_undefined && !value.has_value()) {
		switch (type.get_atom().kind()) {
		case IR::Type::Atom::Kind::Integer:
		case IR::Type::Atom::Kind::Float:
			value
				= {span,
			           IR::Expression::make_atom(
					   IR::Expression::Atom::make_literal(
						   IR::Expression::Atom::Literal::Kind::Number,
						   "0",
						   type.clone()
					   )
				   )};
			break;
		case IR::Type::Atom::Kind::Bool:
			value = {span, IR::Expression::make_atom(IR::Expression::Atom::make_bool(false, type.clone()))};
		case IR::Type::Atom::Kind::Void:  break;
		case IR::Type::Atom::Kind::Error: break;
		case IR::Type::Atom::Kind::Char:
			parsed_files.at(file_id).diagnostics.push_back(
				Diagnostic::error(
					"no default value",
					"this variable's type has no defined default value. if you intended to have an undefined value, specify it by writing '= undefined'",
					{Diagnostic::Sample(get_context(file_id), span, OutFmt::Color::Red)}
				)
			);
		}
	}

	return Spanned<IR::Statement> {
		span,
		IR::Statement::make_declare(
			IR::Statement::Declare {name, std::move(type), std::move(value), declare.mutable_}
		)
	};
}

std::optional<Spanned<IR::Statement>> Resolver::lower(
	AST::Statement::Set const&   set,
	Span                         span,
	std::vector<IR::BasicBlock>& basic_blocks,
	FileContext::ID              file_id
) {
	// skip invalid lhs
	if (!set.lhs.value.can_be_lhs()) return {};
	// TODO: in the future, once we do derefs, this will look different
	auto lhs            = lower(set.lhs, basic_blocks, file_id);
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
	auto value = lower(set.rhs, basic_blocks, file_id);

	return Spanned<IR::Statement> {
		span,
		IR::Statement::make_set(IR::Statement::Set {lhs_identifier, std::move(value)})
	};
}

std::optional<Spanned<IR::Statement>> Resolver::lower(
	Spanned<AST::Statement> const& statement,
	AST::Function&                 function,
	std::vector<IR::BasicBlock>&   basic_blocks,
	FileContext::ID                file_id
) {
	// if we already have a jump and this is not a label, it has to be dead code
	if (!std::holds_alternative<std::monostate>(basic_blocks.at(basic_blocks.size() - 1).jump)
	    && statement.value.kind() != AST::Statement::Kind::Label) {
		parsed_files.at(file_id).diagnostics.push_back(
			Diagnostic::warning(
				"dead code",
				"this statement will never get executed",
				{Diagnostic::Sample(get_context(file_id), statement.span, OutFmt::Color::Yellow)}
			)
		);
		return {};
	}

	switch (statement.value.kind()) {
	case AST::Statement::Kind::Declare:
		return lower(statement.value.get_declare(), statement.span, basic_blocks, file_id);
	case AST::Statement::Kind::Set: return lower(statement.value.get_set(), statement.span, basic_blocks, file_id);
	case AST::Statement::Kind::Scope:
		lower(statement.value.get_scope(), function, basic_blocks, file_id);
		return {};
	case AST::Statement::Kind::Expression:
	case AST::Statement::Kind::Label:
	case AST::Statement::Kind::Return:
	case AST::Statement::Kind::Goto:
	case AST::Statement::Kind::Branch:     break;
	case AST::Statement::Kind::If:         return {};
	}

	// TODO: somehow have a way to insert drop statements before gotos
	if (statement.value.kind() == AST::Statement::Kind::Label) {
		// for labels, we end the current basic block and create a new one for this id
		IR::BasicBlock::ID new_id = statement.value.get_label().id.value();
		// only replace the jump if we need to
		if (std::holds_alternative<std::monostate>(basic_blocks.at(basic_blocks.size() - 1).jump))
			basic_blocks.at(basic_blocks.size() - 1).jump = IR::BasicBlock::Goto {new_id};
		basic_blocks.push_back(IR::BasicBlock {new_id, {}, std::monostate {}});
		return {};
	} else if (statement.value.kind() == AST::Statement::Kind::Return) {
		// for return statements, we set it as our jump
		IR::BasicBlock::Return return_ {
			statement.value.get_return().value.transform([&basic_blocks, file_id, this](auto&& value) {
				return extract_expression(value, basic_blocks, file_id, true);
			})
		};
		// we first compute it and then set it just in case a new basic block got created in the process
		basic_blocks.at(basic_blocks.size() - 1).jump = std::move(return_);
		return {};
	} else if (statement.value.kind() == AST::Statement::Kind::Goto) {
		// goto statements are literally just jumps
		basic_blocks.at(basic_blocks.size() - 1).jump
			= IR::BasicBlock::Goto {statement.value.get_goto().destination_id.value()};
		return {};
	} else if (statement.value.kind() == AST::Statement::Kind::Branch) {
		// branch statements take a bit more effort
		AST::Statement::Branch const& branch = statement.value.get_branch();
		IR::Expression::Atom condition = extract_expression(branch.condition, basic_blocks, file_id).value;
		// if we have a false branch, our job is done
		if (branch.false_.has_value()) {
			basic_blocks.at(basic_blocks.size() - 1).jump = IR::BasicBlock::Branch {
				std::move(condition),
				std::move(branch.true_.value.destination_id.value()),
				branch.false_.value().value.destination_id.value()
			};
			return {};
		}
		// if we don't, we have to manually create one
		AST::Statement::Label::ID new_id              = function.label_counter++;
		basic_blocks.at(basic_blocks.size() - 1).jump = IR::BasicBlock::Branch {
			std::move(condition),
			std::move(branch.true_.value.destination_id.value()),
			new_id
		};
		basic_blocks.push_back(IR::BasicBlock {new_id, {}, std::monostate {}});
		return {};
	}

	// for expressions, it's a special case because we only care about function calls
	// however, when we resolve expressions, we do extract all potential inner function calls
	if (type_pool_.at(statement.value.get_expression().type.value()).kind() == TypeInfo::Kind::Bottom) return {};
	auto expression = lower(statement.value.get_expression(), statement.span, basic_blocks, file_id);
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

	return Spanned<IR::Statement> {expression.span, {std::move(expression.value.get_function_call())}};
}

void Resolver::lower(
	std::vector<Spanned<AST::Statement>> const& statements,
	AST::Function&                              function,
	std::vector<IR::BasicBlock>&                basic_blocks,
	FileContext::ID                             file_id
) {
	for (Spanned<AST::Statement> const& statement : statements) {
		std::optional<Spanned<IR::Statement>> ir_statement = lower(statement, function, basic_blocks, file_id);
		if (ir_statement.has_value())
			basic_blocks.at(basic_blocks.size() - 1).statements.push_back(std::move(ir_statement.value()));
	}
}

IR::Function Resolver::lower(AST::Function& function, FileContext::ID file_id) {
	std::vector<IR::Function::Argument> arguments {};
	arguments.reserve(function.arguments.size());
	for (auto& [name, type, _, mutable_] : function.arguments) {
		// we don't need to push arguments as anonymous or mutable, we only cared during resolution and stuff
		arguments.push_back(
			IR::Function::Argument {lower_identifier(name), lower_type(std::move(type), file_id)}
		);
	}
	Spanned<IR::Type> return_type = lower_type(std::move(function.return_type), file_id);
	// TODO: ensure that we have a return statement at the end, since basic blocks need to always jump
	std::vector<IR::BasicBlock> basic_blocks {};
	if (function.body.has_value()) {
		basic_blocks.push_back(IR::BasicBlock {0, {}, std::monostate {}});
		lower(function.body.value(), function, basic_blocks, file_id);
		// after lowering, we need to ensure that all of these basic blocks are valid IR
		bool returns = std::any_of(
			     basic_blocks.cbegin(),
			     basic_blocks.cend(),
			     [](IR::BasicBlock const& basic_block) {
				     return std::holds_alternative<IR::BasicBlock::Return>(basic_block.jump);
			     }
		     ),
		     needs_to_return = return_type.value.get_atom().kind() != IR::Type::Atom::Kind::Void;
		if (!returns) {
			// if it doesn't need to return, we can add the return manually
			if (!needs_to_return) {
				// this is the only place where we could have a monostate!
				if (std::holds_alternative<std::monostate>(
					    basic_blocks.at(basic_blocks.size() - 1).jump
				    )) {
					// we can automatically add a return void
					basic_blocks.at(basic_blocks.size() - 1).jump
						= IR::BasicBlock::Return {std::nullopt};
				} else {
					// otherwise, this function actually never returns
					parsed_files.at(file_id).diagnostics.push_back(
						Diagnostic::warning(
							"function never returns",
							"there is no way for this function to ever return. if this is intentional, you should mark the function as such (there is no way to do so currently)",
							{Diagnostic::Sample(
								get_context(file_id),
								function.name.span,
								OutFmt::Color::Yellow
							)}
						)
					);
				}
			} else {
				// if it does, we need to add an error. this will prevent codegen from running
				parsed_files.at(file_id).diagnostics.push_back(
					Diagnostic::error(
						"non-void function has no return statement",
						"there is no way for this non-void function to ever return a value",
						{Diagnostic::Sample(
							get_context(file_id),
							function.name.span,
							OutFmt::Color::Red
						)}
					)
				);
			}
		}
	};
	return IR::Function {
		{function.name.span, function.name.value.id.value()[0]},
		std::move(arguments),
		std::move(return_type),
		std::move(basic_blocks),
		false
	};
}

IR::Module Resolver::lower(AST::Module& original_module, FileContext::ID file_id) {
	IR::Module module {lower_identifier(original_module.name), {}};
	// we don't need aliases anymore, those are purely for name resolution
	for (Spanned<AST::Module::Item>& item : original_module.body.items) {
		auto& value = std::get<AST::Module::InnerItem>(item.value);
		if (std::holds_alternative<AST::Function>(value)) {
			AST::Function& function = std::get<AST::Function>(value);
			// TODO: make a more sophisticated system for these kinds of things
			IR::Function lowered_function = lower(function, file_id);
			for (AST::Tag const& tag : std::get<std::vector<AST::Tag>>(item.value)) {
				if (tag.identifier == "extern") lowered_function.extern_ = true;
			}
			get_single_symbol(function.name.value).item = std::move(lowered_function);
			module.items.push_back(function.name.value.id.value()[0]);
		} else if (std::holds_alternative<AST::Module>(value)) {
			AST::Module& submodule                       = std::get<AST::Module>(value);
			get_single_symbol(submodule.name.value).item = lower(submodule, file_id);
			module.items.push_back(submodule.name.value.id.value()[0]);
		}
	}
	return module;
}

std::vector<IR::Module> Resolver::lower() {
	std::vector<IR::Module> files {};
	files.reserve(parsed_files.size());
	std::transform(parsed_files.begin(), parsed_files.end(), std::back_inserter(files), [this](ParsedFile& file) {
		return lower(file.module, file.file_id);
	});
	return files;
}
