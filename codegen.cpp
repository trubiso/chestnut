#include "codegen.hpp"

#include "lexer.hpp"

#include <format>
#include <iostream>
#include <llvm/IR/Constants.h>
#include <llvm/IR/Verifier.h>

void CodeGenerator::process(std::vector<IR::Module> const& modules) {
	create_all_functions(modules);
	emit_all_functions(modules);

	program_.print(llvm::outs(), nullptr);
}

std::string CodeGenerator::get_name(IR::Identifier identifier) const {
	return std::format("{}_{}", symbols_.at(identifier).name, identifier);
}

llvm::Type* CodeGenerator::generate_type(IR::Type const& type) {
	if (type.kind() != IR::Type::Kind::Atom) {
		std::cout << "non-atoms cannot yet be lowered" << std::endl;
		std::exit(0);
	}
	IR::Type::Atom const& atom = type.get_atom();
	switch (atom.kind()) {
	case IR::Type::Atom::Kind::Void:  return builder_.getVoidTy();
	case IR::Type::Atom::Kind::Char:  return builder_.getInt8Ty();
	case IR::Type::Atom::Kind::Bool:  return builder_.getInt1Ty();
	case IR::Type::Atom::Kind::Error: return nullptr;
	case IR::Type::Atom::Kind::Float:
		switch (atom.get_float().width_value()) {
		case 16:  return builder_.getHalfTy();
		case 32:  return builder_.getFloatTy();
		case 64:  return builder_.getDoubleTy();
		case 128: return llvm::Type::getFP128Ty(context_);
		}
		[[assume(false)]];
	case IR::Type::Atom::Kind::Integer:
		switch (atom.get_integer().width_type()) {
		case IR::Type::Atom::Integer::WidthType::Fixed:
			return builder_.getIntNTy(atom.get_integer().bit_width().value());
		case IR::Type::Atom::Integer::WidthType::Ptr:
			return program_.getDataLayout().getIntPtrType(context_, 0);
		case IR::Type::Atom::Integer::WidthType::Size:
			return program_.getDataLayout().getIndexType(context_, 0);
		}
		[[assume(false)]];
	}
}

llvm::Value* CodeGenerator::generate_expression(IR::Expression::Atom const& atom) {
	switch (atom.kind()) {
	case IR::Expression::Atom::Kind::Identifier:
		return builder_.CreateLoad(generate_type(atom.type), variables_[atom.get_identifier()]);
	case IR::Expression::Atom::Kind::Literal: break;
	}

	// literals vary depending on their type
	IR::Expression::Atom::Literal const& literal = atom.get_literal();

	switch (literal.kind) {
	case IR::Expression::Atom::Literal::Kind::Number:
		// this is a bit verbose :P
		switch (atom.type.get_atom().kind()) {
		case IR::Type::Atom::Kind::Integer:
			switch (atom.type.get_atom().get_integer().width_type()) {
			case IR::Type::Atom::Integer::WidthType::Fixed:
				return llvm::ConstantInt::get(
					context_,
					llvm::APInt(
						atom.type.get_atom().get_integer().bit_width().value(),
						literal.literal,
						10
					)
				);
			case IR::Type::Atom::Integer::WidthType::Ptr:
				return llvm::ConstantInt::get(
					context_,
					llvm::APInt(
						program_.getDataLayout().getAddressSizeInBits((unsigned) 0),
						literal.literal,
						10
					)
				);
			case IR::Type::Atom::Integer::WidthType::Size:
				return llvm::ConstantInt::get(
					context_,
					llvm::APInt(
						program_.getDataLayout().getPointerSizeInBits(0),
						literal.literal,
						10
					)
				);
			}
		case IR::Type::Atom::Kind::Float:
			switch (atom.type.get_atom().get_float().width_value()) {
			case 16:
				return llvm::ConstantFP::get(
					context_,
					llvm::APFloat(llvm::APFloat::IEEEhalf(), literal.literal)
				);
			case 32:
				return llvm::ConstantFP::get(
					context_,
					llvm::APFloat(llvm::APFloat::IEEEsingle(), literal.literal)
				);
			case 64:
				return llvm::ConstantFP::get(
					context_,
					llvm::APFloat(llvm::APFloat::IEEEdouble(), literal.literal)
				);
			case 128:
				return llvm::ConstantFP::get(
					context_,
					llvm::APFloat(llvm::APFloat::IEEEquad(), literal.literal)
				);
			}
			[[assume(false)]];
		case IR::Type::Atom::Kind::Void:
		case IR::Type::Atom::Kind::Char:
		case IR::Type::Atom::Kind::Bool:
		case IR::Type::Atom::Kind::Error:
			std::cout << "genuinely what are you doing" << std::endl;
			std::exit(0);
		}

	case IR::Expression::Atom::Literal::Kind::String:
		std::cout << "unsupported string literal lol!!" << std::endl;
		std::exit(0);
	case IR::Expression::Atom::Literal::Kind::Char: break;
	}

	// for chars, we need to look up the codepoint
	// FIXME: this is very primitive
	uint8_t codepoint = 0;
	if (literal.literal.size() == 3) codepoint = literal.literal[1];
	else if (literal.literal.size() == 4) codepoint = Lexer::lookup_escaped(literal.literal[2]).value();
	return builder_.getInt8(codepoint);
}

llvm::Value* CodeGenerator::generate_expression(IR::Expression::FunctionCall const& function_call) {
	llvm::Function*           callee = program_.getFunction(get_name(function_call.callee.value));
	std::vector<llvm::Value*> arguments {};
	for (auto const& argument : function_call.arguments) arguments.push_back(generate_expression(argument.value));
	return builder_.CreateCall(callee, arguments);
}

llvm::Value* CodeGenerator::generate_expression(IR::Expression const& expression) {
	switch (expression.kind()) {
	case IR::Expression::Kind::Atom:         return generate_expression(expression.get_atom());
	case IR::Expression::Kind::FunctionCall: return generate_expression(expression.get_function_call());
	}
}

void CodeGenerator::emit_statement(IR::Statement::Declare const& declare) {
	llvm::Type*  type  = generate_type(declare.type);
	llvm::Value* value = declare.value.has_value() ? generate_expression(declare.value.value().value) : nullptr;
	// TODO: move alloca to beginning of function
	variables_[declare.name.value] = builder_.CreateAlloca(type, nullptr, get_name(declare.name.value));
	if (value) builder_.CreateStore(value, variables_[declare.name.value]);
}

void CodeGenerator::emit_statement(IR::Statement::Set const& set) {
	llvm::Value* value = generate_expression(set.value.value);
	builder_.CreateStore(value, variables_[set.name.value]);
}

void CodeGenerator::emit_statement(IR::Statement::Return const& return_) {
	if (!return_.value.has_value()) {
		builder_.CreateRetVoid();
		return;
	}

	llvm::Value* value = generate_expression(return_.value.value().value);
	builder_.CreateRet(value);
}

void CodeGenerator::emit_statement(IR::Statement const& statement) {
	switch (statement.kind()) {
	case IR::Statement::Kind::Declare: return emit_statement(statement.get_declare());
	case IR::Statement::Kind::Set:     return emit_statement(statement.get_set());
	case IR::Statement::Kind::Call:    generate_expression(statement.get_call()); return;
	case IR::Statement::Kind::Return:  return emit_statement(statement.get_return());
	}
}

void CodeGenerator::emit_function(IR::Function const& ir_function) {
	if (!ir_function.body.has_value()) return;
	llvm::Function* function = program_.getFunction(get_name(ir_function.name.value));
	assert(function);

	llvm::BasicBlock* block = llvm::BasicBlock::Create(context_, "entry", function);
	builder_.SetInsertPoint(block);

	size_t i = 0;
	for (auto& arg : function->args()) {
		AST::SymbolID argument_id   = ir_function.arguments[i++].name.value;
		std::string   argument_name = get_name(argument_id);
		variables_[argument_id]     = builder_.CreateAlloca(arg.getType(), nullptr, argument_name);
		builder_.CreateStore(&arg, variables_[argument_id]);
	}

	for (Spanned<IR::Statement> const& statement : ir_function.body.value()) emit_statement(statement.value);

	llvm::verifyFunction(*function, &llvm::errs());
}

void CodeGenerator::create_function(IR::Function const& function) {
	std::vector<llvm::Type*> argument_types {};
	for (auto const& argument : function.arguments) argument_types.push_back(generate_type(argument.type.value));
	llvm::Type*         return_type   = generate_type(function.return_type.value);
	llvm::FunctionType* function_type = llvm::FunctionType::get(return_type, argument_types, false);

	if (function.extern_) {
		// if it's extern, we want to be able to link against this function, so we keep the name
		llvm::Function* actual_function = llvm::Function::Create(
			function_type,
			llvm::Function::ExternalLinkage,
			symbols_.at(function.name.value).name,
			&program_
		);
		// then, for the rest of it to work, we create an alias to this function
		llvm::GlobalAlias::create(
			function_type,
			0,
			llvm::Function::ExternalLinkage,
			get_name(function.name.value),
			actual_function,
			&program_
		);
		return;
	}

	// under normal circumstances, we just create the function and that's it
	llvm::Function::Create(
		function_type,
		llvm::Function::ExternalLinkage,
		get_name(function.name.value),
		&program_
	);
}

void CodeGenerator::create_all_functions(IR::Module const& module) {
	for (AST::SymbolID item : module.items) {
		if (std::holds_alternative<IR::Module>(symbols_.at(item).item))
			create_all_functions(std::get<IR::Module>(symbols_.at(item).item));
		else if (std::holds_alternative<IR::Function>(symbols_.at(item).item))
			create_function(std::get<IR::Function>(symbols_.at(item).item));
	}
}

void CodeGenerator::create_all_functions(std::vector<IR::Module> const& modules) {
	for (IR::Module const& module : modules) { create_all_functions(module); }
}

void CodeGenerator::emit_all_functions(IR::Module const& module) {
	for (AST::SymbolID item : module.items) {
		if (std::holds_alternative<IR::Module>(symbols_.at(item).item))
			emit_all_functions(std::get<IR::Module>(symbols_.at(item).item));
		else if (std::holds_alternative<IR::Function>(symbols_.at(item).item))
			emit_function(std::get<IR::Function>(symbols_.at(item).item));
	}
}

void CodeGenerator::emit_all_functions(std::vector<IR::Module> const& modules) {
	for (IR::Module const& module : modules) { emit_all_functions(module); }
	llvm::verifyModule(program_, &llvm::errs());
}
