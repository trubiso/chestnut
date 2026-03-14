#include "resolver.hpp"

#include <algorithm>
#include <cstdlib>
#include <iostream>
#include <llvm/ADT/APSInt.h>
#include <sstream>
#include <variant>

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
	case TypeInfo::Kind::PartialFloat:
		return IR::Type::make_atom(IR::Type::Atom::make_float(IR::DEFAULT_FLOAT_WIDTH));
	case TypeInfo::Kind::Unknown:
	case TypeInfo::Kind::Module:
	case TypeInfo::Kind::Function:
	case TypeInfo::Kind::SameAs:
	case TypeInfo::Kind::Generic:
	case TypeInfo::Kind::MemberAccess:
	case TypeInfo::Kind::Named:
	case TypeInfo::Kind::Pointer:
	case TypeInfo::Kind::KnownInteger:
	case TypeInfo::Kind::PartialInteger: break;
	}

	// FIXME: we throw a million diagnostics when there are resolved type cycles
	auto [span, file_id] = type_span_pool_.at(type_origin);
	if (type.is_unknown() || type.is_member_access()) {
		// member accesses shouldn't make it to lowering, but you never know!
		parsed_files.at(file_id).diagnostics.push_back(
			Diagnostic::error(
				"unknown type",
				"this type cannot be inferred automatically",
				{Diagnostic::Sample(get_context(file_id), span, OutFmt::Color::Red)}
			)
		);
		return IR::Type::make_atom(IR::Type::Atom::make_error());
	} else if (type.is_module()) {
		parsed_files.at(file_id).diagnostics.push_back(
			Diagnostic::error(
				"invalid type",
				"modules cannot be used as values",
				{Diagnostic::Sample(get_context(file_id), span, OutFmt::Color::Red)}
			)
		);
		return IR::Type::make_atom(IR::Type::Atom::make_error());
	} else if (type.is_function()) {
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
	} else if (type.is_same_as()) {
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
	} else if (type.is_generic()) {
		return IR::Type::make_atom(
			IR::Type::Atom::make_named(Spanned {get_type_span(type_origin), type.get_generic().name}, {})
		);
	} else if (type.is_named()) {
		assert(type.get_named().candidates().size() == 1 && "this named type should've become a bottom!");
		auto const&     candidate = type.get_named().candidates().at(0);
		IR::GenericList generic_list {};
		generic_list.reserve(candidate.generics.size());
		std::transform(
			candidate.generics.cbegin(),
			candidate.generics.cend(),
			std::back_inserter(generic_list),
			[this](TypeInfo::ID type) {
				return Spanned {get_type_span(type), reconstruct_type(type, true)};
			}
		);
		return IR::Type::make_atom(
			IR::Type::Atom::make_named(
				Spanned {get_type_span(type_origin), candidate.name},
				std::move(generic_list)
			)
		);
	} else if (type.is_pointer()) {
		return IR::Type::make_pointer(
			IR::Type::Pointer {
				std::make_unique<Spanned<IR::Type>>(Spanned {
					get_type_span(type_id),
					reconstruct_type(type.get_pointer().pointee, type_origin)
				}),
				type.get_pointer().mutable_
			}
		);
	} else if (type.is_known_integer()) {
		auto const& integer = type.get_known_integer().integer;
		switch (integer.width_type()) {
		case AST::Type::Atom::Integer::WidthType::Fixed:
			return IR::Type::make_atom(
				IR::Type::Atom::make_integer(integer.bit_width().value(), integer.is_signed())
			);
		case AST::Type::Atom::Integer::WidthType::Ptr:
			return IR::Type::make_atom(
				// FIXME: get size from target!!!
			        // program_.getDataLayout().getAddressSizeInBits((unsigned) 0)
				IR::Type::Atom::make_integer(64, integer.is_signed())
			);
		case AST::Type::Atom::Integer::WidthType::Size:
			return IR::Type::make_atom(
				// FIXME: get size from target!!!
			        // program_.getDataLayout().getPointerSizeInBits(0)
				IR::Type::Atom::make_integer(64, integer.is_signed())
			);
		case AST::Type::Atom::Integer::WidthType::Any: [[assume(false)]]; break;
		}
	} else if (type.is_partial_integer()) {
		// default is signed!
		bool        signed_ = true;
		auto const& integer = type.get_partial_integer().integer;
		if (type.get_partial_integer().signed_is_known) signed_ = integer.is_signed();
		switch (integer.width_type()) {
		case AST::Type::Atom::Integer::WidthType::Any:
		case AST::Type::Atom::Integer::WidthType::Fixed:
			return IR::Type::make_atom(
				IR::Type::Atom::make_integer(
					integer.width_type() == AST::Type::Atom::Integer::WidthType::Any
						? IR::DEFAULT_INTEGER_WIDTH
						: integer.bit_width().value(),
					signed_
				)
			);
		case AST::Type::Atom::Integer::WidthType::Ptr:
			return IR::Type::make_atom(
				// FIXME: get size from target!!!
			        // program_.getDataLayout().getAddressSizeInBits((unsigned) 0)
				IR::Type::Atom::make_integer(64, signed_)
			);
		case AST::Type::Atom::Integer::WidthType::Size:
			return IR::Type::make_atom(
				// FIXME: get size from target!!!
			        // program_.getDataLayout().getPointerSizeInBits(0)
				IR::Type::Atom::make_integer(64, signed_)
			);
		}
	}

	[[assume(false)]];
	return IR::Type::make_atom(IR::Type::Atom::make_error());
}

IR::Type Resolver::reconstruct_type(TypeInfo::ID type_id, bool allow_functions) {
	return reconstruct_type(type_id, type_id, allow_functions);
}

Spanned<IR::Value> Resolver::lower_get_default_value(IR::Type const& type, Span span, FileContext::ID file_id) {
	// TODO: default values for structs (for now, just default value for each field)
	if (!type.is_atom()) goto diagnostic;

	switch (type.get_atom().kind()) {
	case IR::Type::Atom::Kind::Integer:
		return {span,
		        IR::Value::make_atom(
				IR::Value::Atom::make_int_literal(
					llvm::APSInt(llvm::APInt(type.get_atom().get_integer().bit_width(), 0, false)),
					type.clone()
				)
			)};
	case IR::Type::Atom::Kind::Float: {
		// TODO: clean this up somehow
		switch (type.get_atom().get_float().width_value()) {
		case 16:
			return {span,
			        IR::Value::make_atom(
					IR::Value::Atom::make_float_literal(
						llvm::APFloat(llvm::APFloat::IEEEhalf(), 0),
						type.clone()
					)
				)};
		case 32:
			return {span,
			        IR::Value::make_atom(
					IR::Value::Atom::make_float_literal(
						llvm::APFloat(llvm::APFloat::IEEEsingle(), 0),
						type.clone()
					)
				)};

		case 64:
			return {span,
			        IR::Value::make_atom(
					IR::Value::Atom::make_float_literal(
						llvm::APFloat(llvm::APFloat::IEEEdouble(), 0),
						type.clone()
					)
				)};

		case 128:
			return {span,
			        IR::Value::make_atom(
					IR::Value::Atom::make_float_literal(
						llvm::APFloat(llvm::APFloat::IEEEquad(), 0),
						type.clone()
					)
				)};
		default: assert(false);
		}
		assert(false);
	}

	case IR::Type::Atom::Kind::Bool:
		return {span, IR::Value::make_atom(IR::Value::Atom::make_bool_literal(false, type.clone()))};
	case IR::Type::Atom::Kind::Void:
	case IR::Type::Atom::Kind::Error: return {span, IR::Value::make_atom(IR::Value::Atom::make_error())};
	case IR::Type::Atom::Kind::Char:
	case IR::Type::Atom::Kind::Named: goto diagnostic;
	}

diagnostic:
	parsed_files.at(file_id).diagnostics.push_back(
		Diagnostic::error(
			"no default value",
			"this variable's type has no defined default value. if you intended to have an undefined value, specify it by writing `= undefined`",
			{Diagnostic::Sample(get_context(file_id), span, OutFmt::Color::Red)}
		)
	);
	return {span, IR::Value::make_atom(IR::Value::Atom::make_error())};
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
	if (!identifier.is_decided()) { return std::nullopt; }
	AST::SymbolID id = identifier.id();
	return std::tuple {id, reconstruct_type(symbol_pool_.at(id).type, allow_functions)};
}

Spanned<IR::Identifier> Resolver::lower_name(Spanned<AST::Name> const& name) {
	return {name.span, name.value.id.value()};
}

Spanned<std::tuple<IR::Identifier, IR::Type>> Resolver::extract_value_id(
	AST::Expression const&       expression,
	Span                         span,
	std::vector<IR::BasicBlock>& basic_blocks,
	FileContext::ID              file_id,
	bool                         allow_functions
) {
	IR::Type type = reconstruct_type(expression.type.value(), allow_functions);
	// first, we lower the rvalue
	auto value = lower_value(expression, span, basic_blocks, file_id);
	auto name  = extract_value_id(
                std::move(value.value),
                span,
                type.clone(),
                expression.type.value(),
                basic_blocks,
                file_id
        );
	return {
		name.span,
		{std::move(name.value), std::move(type)}
	};
}

Spanned<IR::Identifier> Resolver::extract_value_id(
	IR::Value&&                  expression,
	Span                         span,
	IR::Type&&                   type,
	TypeInfo::ID                 type_id,
	std::vector<IR::BasicBlock>& basic_blocks,
	FileContext::ID              file_id
) {
	AST::SymbolID id = symbol_next();
	symbol_pool_.push_back(Symbol {id, file_id, span, "_", {}, type_id, false, false, {}});
	Spanned<IR::Identifier> name {span, id};

	// we push the declaration
	basic_blocks.at(basic_blocks.size() - 1)
		.statements.emplace_back(
			span,
			IR::Statement::make_declare(
				IR::Statement::Declare {
					name,
					type.clone(),
					{span, false}
        }
			)
		);
	// then we push the set statement
	basic_blocks.at(basic_blocks.size() - 1)
		.statements.emplace_back(
			span,
			IR::Statement::make_set(
				IR::Statement::Set {
					{name.span, IR::Place::make_symbol(name.value, type.clone())},
					{span, std::move(expression)}
        }
			)
		);
	// and we return the declared identifier
	return name;
}

Spanned<IR::Place> Resolver::place_from_value(
	Spanned<IR::Value>&&         value,
	IR::Type&&                   type,
	TypeInfo::ID                 type_id,
	std::vector<IR::BasicBlock>& basic_blocks,
	FileContext::ID              file_id
) {
	// we skip things which are already identifiers
	if (value.value.is_atom() && value.value.get_atom().is_identifier()) {
		return {value.span,
		        IR::Place::make_symbol(
				value.value.get_atom().get_identifier(),
				std::move(value.value.get_atom().type)
			)};
	}
	auto id = extract_value_id(std::move(value.value), value.span, type.clone(), type_id, basic_blocks, file_id);
	return {id.span, IR::Place::make_symbol(id.value, std::move(type))};
}

Spanned<IR::Value::Atom> Resolver::extract_value(
	AST::Expression const&       expression,
	Span                         span,
	std::vector<IR::BasicBlock>& basic_blocks,
	FileContext::ID              file_id,
	bool                         allow_functions
) {
	// we skip things which are already atoms
	if (expression.is_atom())
		return {span,
		        lower_atom(
				expression.get_atom(),
				expression.type.value(),
				span,
				basic_blocks,
				file_id,
				allow_functions
			)};

	auto id = extract_value_id(expression, span, basic_blocks, file_id, allow_functions);
	return {id.span,
	        IR::Value::Atom::make_identifier(std::move(std::get<0>(id.value)), std::move(std::get<1>(id.value)))};
}

Spanned<IR::Value::Atom> Resolver::extract_value(
	Spanned<AST::Expression> const& expression,
	std::vector<IR::BasicBlock>&    basic_blocks,
	FileContext::ID                 file_id,
	bool                            allow_functions
) {
	return extract_value(expression.value, expression.span, basic_blocks, file_id, allow_functions);
}

IR::Value::Atom Resolver::lower_atom(
	AST::Expression::Atom::StructLiteral const& struct_literal,
	TypeInfo::ID                                type_id,
	Span                                        span,
	std::vector<IR::BasicBlock>&                basic_blocks,
	FileContext::ID                             file_id,
	bool                                        allow_functions
) {
	if (!struct_literal.valid) return IR::Value::Atom::make_error();

	IR::Type type = reconstruct_type(type_id);
	if (!type.is_atom() || !type.get_atom().is_named()) return IR::Value::Atom::make_error();
	auto named = std::move(type.get_atom().get_named());

	// since the order of the struct literal's fields is not necessarily the same as the struct's, we need to
	// iterate.

	// PERF: at some point the struct literal's fields should then be replaced by a std::unordered_map
	std::vector<Spanned<IR::Value::Atom>> fields {};
	fields.reserve(struct_literal.fields.size());
	IR::Struct const& struct_ = std::get<IR::Struct>(symbol_pool_.at(named.name.value).item);
	// TODO: exhaustiveness check (we must check that every field exists on the struct, and that every struct field
	// exists on the literal)
	for (auto const& field : struct_.fields) {
		auto corresponding = std::find_if(
			struct_literal.fields.cbegin(),
			struct_literal.fields.cend(),
			[&field](AST::Expression::Atom::StructLiteral::Field const& given_field) {
				return field.name.value == given_field.name.value;
			}
		);
		assert(corresponding != struct_literal.fields.cend());
		// i guess functions are allowed here?
		fields.push_back(extract_value(*corresponding->value, basic_blocks, file_id, true));
	}

	return IR::Value::Atom::make_struct_literal(std::move(named), std::move(fields));
}

IR::Value::Atom Resolver::lower_atom(
	AST::Expression::Atom const& atom,
	TypeInfo::ID                 type_id,
	Span                         span,
	std::vector<IR::BasicBlock>& basic_blocks,
	FileContext::ID              file_id,
	bool                         allow_functions
) {
	// TODO: actually care about allow_functions more LOL
	switch (atom.kind()) {
	case AST::Expression::Atom::Kind::NumberLiteral: {
		auto const& literal = atom.get_number_literal().literal;
		auto        type    = reconstruct_type(type_id);
		assert(type.is_atom());
		if (type.get_atom().is_integer()) {
			// TODO: parse with the correct radix
			return IR::Value::Atom::make_int_literal(
				// literals are always unsigned!
				llvm::APSInt(llvm::APInt(type.get_atom().get_integer().bit_width(), literal, 10), true),
				std::move(type)
			);
		}

		if (type.get_atom().is_float()) {
			switch (type.get_atom().get_float().width_value()) {
			case 16:
				return IR::Value::Atom::make_float_literal(
					llvm::APFloat(llvm::APFloat::IEEEhalf(), literal),
					std::move(type)
				);
				break;
			case 32:
				return IR::Value::Atom::make_float_literal(
					llvm::APFloat(llvm::APFloat::IEEEsingle(), literal),
					std::move(type)
				);
				break;
			case 64:
				return IR::Value::Atom::make_float_literal(
					llvm::APFloat(llvm::APFloat::IEEEdouble(), literal),
					std::move(type)
				);
				break;
			case 128:
				return IR::Value::Atom::make_float_literal(
					llvm::APFloat(llvm::APFloat::IEEEquad(), literal),
					std::move(type)
				);
				break;
			}
		}

		assert(false && "wrong type for integer literal");
	}
	case AST::Expression::Atom::Kind::StringLiteral: assert(false && "unsupported string literal");
	case AST::Expression::Atom::Kind::CharLiteral:   {
		auto const& literal = atom.get_char_literal().literal;
		// for chars, we need to look up the codepoint
		// FIXME: this is very primitive
		uint8_t codepoint = 0;
		if (literal.size() == 3) codepoint = literal[1];
		else if (literal.size() == 4) codepoint = Lexer::lookup_escaped(literal[2]).value();
		return IR::Value::Atom::make_char_literal(codepoint, reconstruct_type(type_id));
	}
	case AST::Expression::Atom::Kind::BoolLiteral:
		return IR::Value::Atom::make_bool_literal(atom.get_bool_literal().value, reconstruct_type(type_id));
	case AST::Expression::Atom::Kind::StructLiteral:
		return lower_atom(atom.get_struct_literal(), type_id, span, basic_blocks, file_id, allow_functions);
	case AST::Expression::Atom::Kind::Expression:
		return extract_value(*atom.get_expression(), span, basic_blocks, file_id).value;
	case AST::Expression::Atom::Kind::Identifier: break;
	}

	// for identifiers, we need to extract the type as well.
	// only identifiers can be functions!
	auto data = lower(atom.get_identifier(), allow_functions);
	if (!data.has_value()) return IR::Value::Atom::make_error();
	auto [identifier, type] = std::move(data.value());
	return IR::Value::Atom::make_identifier(identifier, std::move(type));
}

Spanned<IR::Value> Resolver::lower_value(
	AST::Expression::Atom const& atom,
	TypeInfo::ID                 type_id,
	Span                         span,
	std::vector<IR::BasicBlock>& basic_blocks,
	FileContext::ID              file_id,
	bool                         allow_functions
) {
	return {span, IR::Value::make_atom(lower_atom(atom, type_id, span, basic_blocks, file_id, allow_functions))};
}

Spanned<IR::Value> Resolver::lower_value(
	AST::Expression::FunctionCall& function_call,
	Span                           span,
	std::vector<IR::BasicBlock>&   basic_blocks,
	FileContext::ID                file_id,
	bool                           allow_functions
) {
	auto error = Spanned<IR::Value> {span, IR::Value::make_atom(IR::Value::Atom::make_error())};

	if (!function_call.call_type.has_value()) return error;

	auto callee = lower_value(*function_call.callee, basic_blocks, file_id, true);
	// if the callee is not valid, we've already thrown diagnostics about it
	if (!callee.value.is_atom() || !callee.value.get_atom().is_identifier()) return error;
	Spanned<IR::Identifier> callee_identifier {callee.span, callee.value.get_atom().get_identifier()};

	// now we need to reconstruct the argument order from the type
	// these assertions shouldn't fail if we've successfully resolved, but you never know!
	TypeInfo::ID function_id = symbol_pool_.at(callee_identifier.value).type;
	if (!type_pool_.at(function_id).is_function()) return error;
	TypeInfo::Function const& function = type_pool_.at(function_id).get_function();
	size_t argument_count = function_call.arguments.ordered.size() + function_call.arguments.labeled.size();
	assert(function.arguments.size() == argument_count);
	std::vector<Spanned<IR::Value::Atom>> arguments {};
	arguments.reserve(argument_count);
	// ordered arguments are freebies
	std::transform(
		function_call.arguments.ordered.cbegin(),
		function_call.arguments.ordered.cend(),
		std::back_inserter(arguments),
		[this, &basic_blocks, file_id](auto const& ordered_argument) {
			return extract_value(ordered_argument, basic_blocks, file_id);
		}
	);
	// labeled arguments have to be reordered according to the function call type
	for (size_t i = function_call.arguments.ordered.size(); i < argument_count; ++i) {
		assert(std::get<0>(function.arguments.at(i)).has_value());
		std::string argument_name  = std::get<0>(function.arguments.at(i)).value();
		bool        argument_found = false;
		for (auto const& [label, argument] : function_call.arguments.labeled) {
			if (label.value == argument_name) {
				arguments.push_back(extract_value(argument, basic_blocks, file_id));
				argument_found = true;
			}
		}
		assert(argument_found);
	}
	assert(arguments.size() == argument_count);

	// then we need to reconstruct the generic order from the type
	IR::GenericList generic_list {};
	auto const&     function_generics = type_pool_.at(function_call.call_type.value()).get_function().generics;
	if (!function_generics.empty()) {
		assert(function.generics.size() == function_generics.size());
		generic_list.reserve(function_generics.size());

		size_t ordered_amt = function_call.generic_list.has_value()
		                           ? function_call.generic_list.value().ordered.size()
		                           : 0;
		size_t total_amt   = function_call.generic_list.has_value()
		                           ? (ordered_amt + function_call.generic_list.value().labeled.size())
		                           : 0;

		for (size_t i = 0; i < function_generics.size(); ++i) {
			auto type = reconstruct_type(std::get<1>(function_generics.at(i)));
			if (i < ordered_amt) {
				// ordered generics are freebies
				generic_list.emplace_back(
					function_call.generic_list.value().ordered.at(i).span,
					std::move(type)
				);
			} else if (i < total_amt) {
				// we must find the labeled generic span
				assert(std::get<0>(function.generics.at(i)).has_value());
				std::string generic_name  = std::get<0>(function.generics.at(i)).value();
				bool        generic_found = false;
				Span        generic_span(0);
				for (auto& [label, generic] : function_call.generic_list.value().labeled) {
					if (label.value == generic_name) {
						assert(!generic_found);
						generic_span  = generic.span;
						generic_found = true;
					}
				}
				assert(generic_found);
				generic_list.emplace_back(generic_span, std::move(type));
			} else {
				generic_list.emplace_back(span, std::move(type));
			}
		}
		assert(generic_list.size() == function.generics.size());
	}

	// finally, we have the callee and the arguments
	// TODO: check if the result is a function for allow_functions
	return {span,
	        IR::Value::make_function_call(
			std::move(callee_identifier),
			std::move(generic_list),
			std::move(arguments)
		)};
}

Spanned<IR::Value> Resolver::lower_value(
	AST::Expression::AddressOperation const& address_operation,
	Span                                     span,
	std::vector<IR::BasicBlock>&             basic_blocks,
	FileContext::ID                          file_id,
	bool                                     allow_functions
) {
	auto operand = lower_place(*address_operation.operand, basic_blocks, file_id);
	return Spanned<IR::Value> {span, IR::Value::make_ref(std::move(operand), address_operation.mutable_)};
}

Spanned<IR::Value> Resolver::lower_value(
	AST::Expression const&       expression,
	Span                         span,
	std::vector<IR::BasicBlock>& basic_blocks,
	FileContext::ID              file_id,
	bool                         allow_functions
) {
	switch (expression.kind()) {
	case AST::Expression::Kind::Atom:
		return lower_value(
			expression.get_atom(),
			expression.type.value(),
			span,
			basic_blocks,
			file_id,
			allow_functions
		);
	case AST::Expression::Kind::FunctionCall:
		// this const cast is ugly, but at least we don't change every function due to it :P
		return lower_value(
			const_cast<AST::Expression::FunctionCall&>(expression.get_function_call()),
			span,
			basic_blocks,
			file_id,
			allow_functions
		);
	case AST::Expression::Kind::MemberAccess:
		return {span,
		        IR::Value::make_load(lower_place(
				expression.get_member_access(),
				expression.type.value(),
				span,
				basic_blocks,
				file_id,
				allow_functions
			))};
	case AST::Expression::Kind::UnaryOperation:
		return Spanned<IR::Value> {
			span,
			IR::Value::make_load(lower_place(expression, span, basic_blocks, file_id, allow_functions))
		};
	case AST::Expression::Kind::AddressOperation:
		return lower_value(expression.get_address_operation(), span, basic_blocks, file_id, allow_functions);
	case AST::Expression::Kind::BinaryOperation:
	case AST::Expression::Kind::If:              [[assume(false)]];
	}
	assert(false);
}

Spanned<IR::Value> Resolver::lower_value(
	Spanned<AST::Expression> const& expression,
	std::vector<IR::BasicBlock>&    basic_blocks,
	FileContext::ID                 file_id,
	bool                            allow_functions
) {
	return lower_value(expression.value, expression.span, basic_blocks, file_id, allow_functions);
}

Spanned<IR::Place> Resolver::lower_place(
	AST::Expression::AddressOperation const& address_operation,
	TypeInfo::ID                             type_id,
	Span                                     span,
	std::vector<IR::BasicBlock>&             basic_blocks,
	FileContext::ID                          file_id,
	bool                                     allow_functions
) {
	return place_from_value(
		lower_value(address_operation, span, basic_blocks, file_id, allow_functions),
		reconstruct_type(type_id),
		type_id,
		basic_blocks,
		file_id
	);
}

Spanned<IR::Place> Resolver::lower_place(
	AST::Expression::FunctionCall const& function_call,
	TypeInfo::ID                         type_id,
	Span                                 span,
	std::vector<IR::BasicBlock>&         basic_blocks,
	FileContext::ID                      file_id,
	bool                                 allow_functions
) {
	// this const cast is ugly, but at least we don't change every function due to it :P
	return place_from_value(
		lower_value(
			const_cast<AST::Expression::FunctionCall&>(function_call),
			span,
			basic_blocks,
			file_id,
			allow_functions
		),
		reconstruct_type(type_id),
		type_id,
		basic_blocks,
		file_id
	);
}

Spanned<IR::Place> Resolver::lower_place(
	AST::Expression::UnaryOperation const& unary_operation,
	TypeInfo::ID                           type_id,
	Span                                   span,
	std::vector<IR::BasicBlock>&           basic_blocks,
	FileContext::ID                        file_id,
	bool                                   allow_functions
) {
	assert(unary_operation.operation == Token::Symbol::Star);
	auto operand = lower_place(*unary_operation.operand, basic_blocks, file_id);
	return Spanned<IR::Place> {
		span,
		IR::Place::make_deref(
			std::make_unique<Spanned<IR::Place>>(std::move(operand)),
			reconstruct_type(type_id)
		)
	};
}

Spanned<IR::Place> Resolver::lower_place(
	AST::Expression::Atom const& atom,
	TypeInfo::ID                 type_id,
	Span                         span,
	std::vector<IR::BasicBlock>& basic_blocks,
	FileContext::ID              file_id,
	bool                         allow_functions
) {
	return place_from_value(
		lower_value(atom, type_id, span, basic_blocks, file_id, allow_functions),
		reconstruct_type(type_id),
		type_id,
		basic_blocks,
		file_id
	);
}

Spanned<IR::Place> Resolver::lower_place(
	AST::Expression::MemberAccess const& member_access,
	TypeInfo::ID                         type_id,
	Span                                 span,
	std::vector<IR::BasicBlock>&         basic_blocks,
	FileContext::ID                      file_id,
	bool                                 allow_functions
) {
	auto error_place
		= Spanned<IR::Place> {span, IR::Place::make_error(IR::Type::make_atom(IR::Type::Atom::make_error()))};

	auto accessee = lower_place(*member_access.accessee, basic_blocks, file_id);

	if (!accessee.value.type.is_atom() || !accessee.value.type.get_atom().is_named()) return error_place;
	// FIXME: this completely discards generics
	IR::Identifier    struct_id   = accessee.value.type.get_atom().get_named().name.value;
	IR::Struct const& struct_     = std::get<IR::Struct>(symbol_pool_.at(struct_id).item);
	size_t            field_index = 0;
	// we know the field is in there somewhere
	// TODO: assert that!
	for (size_t i = 0; i < struct_.fields.size(); ++i) {
		if (struct_.fields[i].name.value == member_access.field.value) {
			field_index = i;
			break;
		}
	}

	IR::Type type = reconstruct_type(type_id);

	// TODO: check if it is a function (it shouldn't be)
	return {span,
	        IR::Place::make_access(
			std::make_unique<Spanned<IR::Place>>(std::move(accessee)),
			field_index,
			std::move(type)
		)};
}

Spanned<IR::Place> Resolver::lower_place(
	AST::Expression const&       expression,
	Span                         span,
	std::vector<IR::BasicBlock>& basic_blocks,
	FileContext::ID              file_id,
	bool                         allow_functions
) {
	switch (expression.kind()) {
	case AST::Expression::Kind::Atom:
		return lower_place(
			expression.get_atom(),
			expression.type.value(),
			span,
			basic_blocks,
			file_id,
			allow_functions
		);
	case AST::Expression::Kind::FunctionCall:
		return lower_place(
			expression.get_function_call(),
			expression.type.value(),
			span,
			basic_blocks,
			file_id,
			allow_functions
		);
	case AST::Expression::Kind::MemberAccess:
		return lower_place(
			expression.get_member_access(),
			expression.type.value(),
			span,
			basic_blocks,
			file_id,
			allow_functions
		);
	case AST::Expression::Kind::UnaryOperation:
		return lower_place(
			expression.get_unary_operation(),
			expression.type.value(),
			span,
			basic_blocks,
			file_id,
			allow_functions
		);
	case AST::Expression::Kind::AddressOperation:
		return lower_place(
			expression.get_address_operation(),
			expression.type.value(),
			span,
			basic_blocks,
			file_id,
			allow_functions
		);
	case AST::Expression::Kind::BinaryOperation:
	case AST::Expression::Kind::If:              [[assume(false)]];
	}
	assert(false);
}

Spanned<IR::Place> Resolver::lower_place(
	Spanned<AST::Expression> const& expression,
	std::vector<IR::BasicBlock>&    basic_blocks,
	FileContext::ID                 file_id,
	bool                            allow_functions
) {
	return lower_place(expression.value, expression.span, basic_blocks, file_id, allow_functions);
}

std::optional<Spanned<IR::Statement>> Resolver::lower(
	AST::Statement::Declare const& declare,
	Span                           span,
	std::vector<IR::BasicBlock>&   basic_blocks,
	FileContext::ID                file_id
) {
	assert(declare.name.value.id.has_value());

	auto name = lower_name(declare.name);
	auto type = reconstruct_type(symbol_pool_.at(name.value).type, false);

	auto value = declare.value.transform([&basic_blocks, file_id, this](auto&& value) {
		return lower_value(value, basic_blocks, file_id);
	});

	if (declare.is_undefined) assert(!value.has_value());

	// value will only be {} if it is undefined
	if (!declare.is_undefined && !value.has_value()) value = lower_get_default_value(type, span, file_id);

	// we add the declare first
	basic_blocks.at(basic_blocks.size() - 1)
		.statements.emplace_back(
			span,
			IR::Statement::make_declare(
				IR::Statement::Declare {name, type.clone(), std::move(declare.mutable_)}
			)
		);

	// then, we add the set statement if we have a value
	if (!value.has_value()) return {};
	return Spanned<IR::Statement> {
		span,
		IR::Statement::make_set(
			IR::Statement::Set {
					    Spanned<IR::Place> {
					declare.name.span,
					IR::Place::make_symbol(name.value, std::move(type))
				}, std::move(value.value())
			}
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
	auto lhs   = lower_place(set.lhs, basic_blocks, file_id);
	auto value = lower_value(set.rhs, basic_blocks, file_id);

	return Spanned<IR::Statement> {
		span,
		IR::Statement::make_set(IR::Statement::Set {std::move(lhs), std::move(value)})
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
	    && !statement.value.is_label()) {
		// do not push diagnostics for automatically generated statements
		if (!statement.value.is_auto_generated)
			parsed_files.at(file_id).diagnostics.push_back(
				Diagnostic::warning(
					"dead code",
					"this statement will never get executed",
					{Diagnostic::Sample(
						get_context(file_id),
						statement.span,
						OutFmt::Color::Yellow
					)}
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
	case AST::Statement::Kind::If:
	case AST::Statement::Kind::While:
	case AST::Statement::Kind::Break:
	case AST::Statement::Kind::Continue:   return {};
	}

	// TODO: somehow have a way to insert drop statements before gotos
	if (statement.value.is_label()) {
		// for labels, we end the current basic block and create a new one for this id
		IR::BasicBlock::ID new_id = statement.value.get_label().id.value();
		// only replace the jump if we need to
		if (std::holds_alternative<std::monostate>(basic_blocks.at(basic_blocks.size() - 1).jump))
			basic_blocks.at(basic_blocks.size() - 1).jump = IR::BasicBlock::Goto {new_id};
		basic_blocks.push_back(IR::BasicBlock {new_id, {}, std::monostate {}});
		return {};
	} else if (statement.value.is_return()) {
		// for return statements, we set it as our jump
		IR::BasicBlock::Return return_ {
			statement.value.get_return().value.transform([&basic_blocks, file_id, this](auto&& value) {
				return extract_value(value, basic_blocks, file_id, true);
			})
		};
		// we first compute it and then set it just in case a new basic block got created in the process
		basic_blocks.at(basic_blocks.size() - 1).jump = std::move(return_);
		return {};
	} else if (statement.value.is_goto()) {
		// goto statements are literally just jumps
		basic_blocks.at(basic_blocks.size() - 1).jump
			= IR::BasicBlock::Goto {statement.value.get_goto().destination_id.value()};
		return {};
	} else if (statement.value.is_branch()) {
		// branch statements take a bit more effort
		AST::Statement::Branch const& branch    = statement.value.get_branch();
		Spanned<IR::Value::Atom>      condition = extract_value(branch.condition, basic_blocks, file_id);
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
	if (type_pool_.at(statement.value.get_expression().type.value()).is_bottom()) return {};
	auto expression = lower_value(statement.value.get_expression(), statement.span, basic_blocks, file_id);
	if (!expression.value.is_function_call()) {
		if (expression.value.is_atom() && expression.value.get_atom().is_error()) return {};

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

IR::GenericDeclaration
Resolver::lower(std::optional<AST::GenericDeclaration>& maybe_generic_declaration, FileContext::ID file_id) {
	if (!maybe_generic_declaration.has_value()) return {};
	IR::GenericDeclaration new_declaration {};

	auto& generics = maybe_generic_declaration.value().generics;
	new_declaration.reserve(generics.size());
	for (auto& generic : generics) {
		std::vector<IR::Generic::Constraint> constraints {};
		assert(generic.name.value.id.has_value());
		auto const& symbol = get_single_symbol(generic.name.value);
		if (type_pool_.at(symbol.type).is_bottom()) goto bail;

		{
			auto const& trait_constraints = type_pool_.at(symbol.type).get_generic().declared_constraints;

			// PERF: we should maybe pre-expand these beforehand instead of doing it so many times
			std::vector<TypeInfo::Generic::TraitConstraint> expanded_list {};

			for (auto const& trait_constraint : trait_constraints) {
				std::vector<TypeInfo::Generic::TraitConstraint> expanded
					= expand_trait(trait_constraint);
				std::move(expanded.begin(), expanded.end(), std::back_inserter(expanded_list));
			}

			auto maybe_expanded = reduce_to_unique(std::move(expanded_list));
			if (!maybe_expanded.has_value()) goto bail;
			expanded_list = std::move(maybe_expanded.value());

			constraints.reserve(expanded_list.size());
			std::transform(
				expanded_list.begin(),
				expanded_list.end(),
				std::back_inserter(constraints),
				[this](TypeInfo::Generic::TraitConstraint& constraint) {
					IR::GenericList generic_list {};
					generic_list.reserve(constraint.arguments.size());
					std::transform(
						constraint.arguments.cbegin(),
						constraint.arguments.cend(),
						std::back_inserter(generic_list),
						[this](TypeInfo::ID type_id) {
							return Spanned {
								get_type_span(type_id),
								reconstruct_type(type_id)
							};
						}
					);
					return IR::Generic::Constraint {constraint.name, std::move(generic_list)};
				}
			);
		}
	bail:
		new_declaration.emplace_back(generic.name.value.id.value(), std::move(constraints));
	}

	return new_declaration;
}

IR::Function Resolver::lower(AST::Function& function, FileContext::ID file_id) {
	auto const& function_type = type_pool_.at(get_single_symbol(function.name.value).type).get_function();
	std::vector<IR::Function::Argument> arguments {};
	arguments.reserve(function.arguments.size());
	for (auto& [name, type, _, mutable_] : function.arguments) {
		// we don't need to push arguments as anonymous or mutable, we only cared during resolution and stuff
		arguments.push_back(
			IR::Function::Argument {
				lower_name(name),
				{type.span, reconstruct_type(get_single_symbol(name.value).type)}
                }
		);
	}
	Spanned<IR::Type>           return_type = {function.return_type.span, reconstruct_type(function_type.return_)};
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
		     needs_to_return = !return_type.value.get_atom().is_void();
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
	auto generic_declaration = lower(function.generic_declaration, file_id);
	return IR::Function {
		{function.name.span, function.name.value.id.value()},
		std::move(generic_declaration),
		std::move(arguments),
		std::move(return_type),
		std::move(basic_blocks),
		false
	};
}

IR::Struct Resolver::lower(AST::Struct& struct_, FileContext::ID file_id) {
	std::vector<IR::Struct::Field> fields {};
	fields.reserve(struct_.fields.size());
	std::transform(
		struct_.fields.begin(),
		struct_.fields.end(),
		std::back_inserter(fields),
		[&](AST::Struct::Field& field) {
			assert(field.type_id.has_value() && "we missed a struct during type inference");
			return IR::Struct::Field {
				Spanned {field.name.span,                        field.name.value},
				{field.type.span, reconstruct_type(field.type_id.value())}
			};
		}
	);

	auto generic_declaration = lower(struct_.generic_declaration, file_id);
	return IR::Struct {lower_name(struct_.name), std::move(generic_declaration), std::move(fields)};
}

IR::Module Resolver::lower(AST::Module& original_module, FileContext::ID file_id) {
	IR::Module module {lower_name(original_module.name), {}};
	// we don't need aliases anymore, those are purely for name resolution
	// FIXME: ensure structs are lowered before we resolve function bodies
	for (Spanned<AST::Module::Item>& item : original_module.body.items) {
		auto& value = std::get<AST::Module::InnerItem>(item.value);
		if (std::holds_alternative<AST::Function>(value)) {
			AST::Function& function = std::get<AST::Function>(value);
			if (!type_pool_.at(get_single_symbol(function.name.value).type).is_function()) continue;
			// TODO: make a more sophisticated system for these kinds of things
			IR::Function lowered_function = lower(function, file_id);
			for (AST::Tag const& tag : std::get<std::vector<AST::Tag>>(item.value)) {
				if (tag.identifier == "extern") lowered_function.extern_ = true;
			}
			get_single_symbol(function.name.value).item = std::move(lowered_function);
			module.items.push_back(function.name.value.id.value());
		} else if (std::holds_alternative<AST::Module>(value)) {
			AST::Module& submodule                       = std::get<AST::Module>(value);
			get_single_symbol(submodule.name.value).item = lower(submodule, file_id);
			module.items.push_back(submodule.name.value.id.value());
		} else if (std::holds_alternative<AST::Struct>(value)) {
			AST::Struct& struct_                       = std::get<AST::Struct>(value);
			IR::Struct   lowered_struct_               = lower(struct_, file_id);
			get_single_symbol(struct_.name.value).item = std::move(lowered_struct_);
			module.items.push_back(struct_.name.value.id.value());
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
