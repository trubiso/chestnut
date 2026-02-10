#include "codegen.hpp"

#include "lexer.hpp"

#include <format>
#include <iostream>
#include <llvm/IR/Constants.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/PassManager.h>
#include <llvm/IR/Verifier.h>
#include <llvm/MC/TargetRegistry.h>
#include <llvm/Passes/PassBuilder.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/Target/TargetOptions.h>
#include <llvm/TargetParser/Host.h>
#include <llvm/Transforms/Utils/Mem2Reg.h>

void CodeGenerator::process(std::vector<IR::Module> const& modules, std::string of, Optimization optimization) {
	create_all_functions(modules);
	emit_all_functions(modules);

	// TODO: move all of this to main and make this more sophisticated
	auto target_triple = llvm::sys::getDefaultTargetTriple();
	llvm::InitializeAllTargetInfos();
	llvm::InitializeAllTargets();
	llvm::InitializeAllTargetMCs();
	llvm::InitializeAllAsmParsers();
	llvm::InitializeAllAsmPrinters();

	std::string error;

	llvm::Target const* target = llvm::TargetRegistry::lookupTarget(target_triple, error);

	if (!target) {
		llvm::errs() << error;
		std::exit(1);
	}

	auto cpu      = "generic";
	auto features = "";

	llvm::TargetOptions opt;
	auto target_machine = target->createTargetMachine(target_triple, cpu, features, opt, llvm::Reloc::PIC_);

	program_.setDataLayout(target_machine->createDataLayout());

	llvm::LoopAnalysisManager     lam;
	llvm::FunctionAnalysisManager fam;
	llvm::CGSCCAnalysisManager    cgam;
	llvm::ModuleAnalysisManager   mam;

	llvm::PassBuilder pb(target_machine);

	pb.registerModuleAnalyses(mam);
	pb.registerCGSCCAnalyses(cgam);
	pb.registerFunctionAnalyses(fam);
	pb.registerLoopAnalyses(lam);
	pb.crossRegisterProxies(lam, fam, cgam, mam);

	llvm::OptimizationLevel optimization_level = llvm::OptimizationLevel::O0;
	switch (optimization) {
	case Optimization::O0: optimization_level = llvm::OptimizationLevel::O0; break;
	case Optimization::O1: optimization_level = llvm::OptimizationLevel::O1; break;
	case Optimization::O2: optimization_level = llvm::OptimizationLevel::O2; break;
	case Optimization::O3: optimization_level = llvm::OptimizationLevel::O3; break;
	}
	llvm::ModulePassManager mpm = pb.buildPerModuleDefaultPipeline(optimization_level);
	// ensure we always run the alloca pass!
	mpm.addPass(llvm::createModuleToFunctionPassAdaptor(llvm::PromotePass()));

	mpm.run(program_, mam);

	program_.print(llvm::outs(), nullptr);

	std::error_code      ec;
	llvm::raw_fd_ostream dest(of, ec, llvm::sys::fs::OF_None);

	if (ec) {
		llvm::errs() << "Could not open file: " << ec.message();
		std::exit(1);
	}

	llvm::legacy::PassManager pass;

	auto file_type = llvm::CodeGenFileType::ObjectFile;

	if (target_machine->addPassesToEmitFile(pass, dest, nullptr, file_type)) {
		llvm::errs() << "TargetMachine can't emit a file of this type";
		std::exit(1);
	}

	pass.run(program_);
	dest.flush();
}

std::string CodeGenerator::get_name(IR::Identifier identifier) const {
	return std::format("{}_{}", symbols_.at(identifier).name, identifier);
}

std::string CodeGenerator::get_block_name(IR::BasicBlock::ID id) const {
	return std::format("_{}b", id);
}

llvm::Type* CodeGenerator::generate_type(IR::Type::Atom const& atom) {
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

llvm::Type* CodeGenerator::generate_type(IR::Type const& type) {
	switch (type.kind()) {
	case IR::Type::Kind::Atom:    return generate_type(type.get_atom());
	case IR::Type::Kind::Pointer: return builder_.getPtrTy();
	}
}

llvm::Value* CodeGenerator::call_built_in(
	IR::BuiltInFunction                               function,
	std::vector<Spanned<IR::Expression::Atom>> const& function_arguments
) {
	std::vector<llvm::Value*> arguments {};
	std::transform(
		function_arguments.cbegin(),
		function_arguments.cend(),
		std::back_inserter(arguments),
		[this](auto const& argument) { return generate_expression(argument.value); }
	);
	switch (function) {
	case IR::BuiltInFunction::AddUIntegers:
		assert(arguments.size() == 2);
		return builder_.CreateAdd(arguments[0], arguments[1], "", false, true);
	case IR::BuiltInFunction::AddSIntegers:
		assert(arguments.size() == 2);
		return builder_.CreateAdd(arguments[0], arguments[1], "", true, false);
	case IR::BuiltInFunction::AddFloats:
		assert(arguments.size() == 2);
		return builder_.CreateFAdd(arguments[0], arguments[1]);
	case IR::BuiltInFunction::SubtractUIntegers:
		assert(arguments.size() == 2);
		return builder_.CreateSub(arguments[0], arguments[1], "", false, true);
	case IR::BuiltInFunction::SubtractSIntegers:
		assert(arguments.size() == 2);
		return builder_.CreateSub(arguments[0], arguments[1], "", true, false);
	case IR::BuiltInFunction::SubtractFloats:
		assert(arguments.size() == 2);
		return builder_.CreateFSub(arguments[0], arguments[1]);
	case IR::BuiltInFunction::MultiplyUIntegers:
		assert(arguments.size() == 2);
		return builder_.CreateMul(arguments[0], arguments[1], "", false, true);
	case IR::BuiltInFunction::MultiplySIntegers:
		assert(arguments.size() == 2);
		return builder_.CreateMul(arguments[0], arguments[1], "", true, false);
	case IR::BuiltInFunction::MultiplyFloats:
		assert(arguments.size() == 2);
		return builder_.CreateFMul(arguments[0], arguments[1]);
	case IR::BuiltInFunction::DivideUIntegers:
		assert(arguments.size() == 2);
		return builder_.CreateUDiv(arguments[0], arguments[1]);
	case IR::BuiltInFunction::DivideSIntegers:
		assert(arguments.size() == 2);
		return builder_.CreateSDiv(arguments[0], arguments[1]);
	case IR::BuiltInFunction::DivideFloats:
		assert(arguments.size() == 2);
		return builder_.CreateFDiv(arguments[0], arguments[1]);
	case IR::BuiltInFunction::NegateSInteger:
		// since there is no built-in function, we do 0-x
		assert(arguments.size() == 1);
		return builder_.CreateSub(
			builder_.getIntN(
				function_arguments[0].value.type.get_atom().get_integer().bit_width().value(),
				0
			),
			arguments[0],
			"",
			true,
			false
		);
	case IR::BuiltInFunction::NegateFloat: assert(arguments.size() == 1); return builder_.CreateFNeg(arguments[0]);
	case IR::BuiltInFunction::NegateBool:
		// since there is no built-in function, we compare against false
		assert(arguments.size() == 1);
		return builder_.CreateICmpEQ(arguments[0], builder_.getInt1(false));
	case IR::BuiltInFunction::EqIntegers:
		assert(arguments.size() == 2);
		return builder_.CreateICmpEQ(arguments[0], arguments[1]);
	case IR::BuiltInFunction::EqFloats:
		assert(arguments.size() == 2);
		return builder_.CreateFCmpOEQ(arguments[0], arguments[1]);
	case IR::BuiltInFunction::EqChars:
		assert(arguments.size() == 2);
		return builder_.CreateICmpEQ(arguments[0], arguments[1]);
	case IR::BuiltInFunction::EqBools:
		assert(arguments.size() == 2);
		return builder_.CreateICmpEQ(arguments[0], arguments[1]);
	case IR::BuiltInFunction::NeIntegers:
		assert(arguments.size() == 2);
		return builder_.CreateICmpNE(arguments[0], arguments[1]);
	case IR::BuiltInFunction::NeFloats:
		assert(arguments.size() == 2);
		return builder_.CreateFCmpONE(arguments[0], arguments[1]);
	case IR::BuiltInFunction::NeChars:
		assert(arguments.size() == 2);
		return builder_.CreateICmpNE(arguments[0], arguments[1]);
	case IR::BuiltInFunction::NeBools:
		assert(arguments.size() == 2);
		return builder_.CreateICmpNE(arguments[0], arguments[1]);
	case IR::BuiltInFunction::GtUIntegers:
		assert(arguments.size() == 2);
		return builder_.CreateICmpUGT(arguments[0], arguments[1]);
	case IR::BuiltInFunction::GtSIntegers:
		assert(arguments.size() == 2);
		return builder_.CreateICmpSGT(arguments[0], arguments[1]);
	case IR::BuiltInFunction::GtFloats:
		assert(arguments.size() == 2);
		return builder_.CreateFCmpOGT(arguments[0], arguments[1]);
	case IR::BuiltInFunction::GeUIntegers:
		assert(arguments.size() == 2);
		return builder_.CreateICmpUGE(arguments[0], arguments[1]);
	case IR::BuiltInFunction::GeSIntegers:
		assert(arguments.size() == 2);
		return builder_.CreateICmpSGE(arguments[0], arguments[1]);
	case IR::BuiltInFunction::GeFloats:
		assert(arguments.size() == 2);
		return builder_.CreateFCmpOGE(arguments[0], arguments[1]);
	case IR::BuiltInFunction::LtUIntegers:
		assert(arguments.size() == 2);
		return builder_.CreateICmpULT(arguments[0], arguments[1]);
	case IR::BuiltInFunction::LtSIntegers:
		assert(arguments.size() == 2);
		return builder_.CreateICmpSLT(arguments[0], arguments[1]);
	case IR::BuiltInFunction::LtFloats:
		assert(arguments.size() == 2);
		return builder_.CreateFCmpOLT(arguments[0], arguments[1]);
	case IR::BuiltInFunction::LeUIntegers:
		assert(arguments.size() == 2);
		return builder_.CreateICmpULE(arguments[0], arguments[1]);
	case IR::BuiltInFunction::LeSIntegers:
		assert(arguments.size() == 2);
		return builder_.CreateICmpSLE(arguments[0], arguments[1]);
	case IR::BuiltInFunction::LeFloats:
		assert(arguments.size() == 2);
		return builder_.CreateFCmpOLE(arguments[0], arguments[1]);
	}
}

llvm::Value* CodeGenerator::generate_expression(IR::Expression::Atom const& atom) {
	switch (atom.kind()) {
	case IR::Expression::Atom::Kind::Identifier:
		return builder_.CreateLoad(generate_type(atom.type), variables_[atom.get_identifier()]);
	case IR::Expression::Atom::Kind::Literal: break;
	case IR::Expression::Atom::Kind::Bool:    return builder_.getInt1(atom.get_bool());
	case IR::Expression::Atom::Kind::Error:   [[assume(false)]];
	}

	// literals vary depending on their type
	IR::Expression::Atom::Literal const& literal = atom.get_literal();

	switch (literal.kind) {
	case IR::Expression::Atom::Literal::Kind::Number:
		// this is a bit verbose :P
		switch (atom.type.get_atom().kind()) {
		case IR::Type::Atom::Kind::Integer:
			switch (atom.type.get_atom().get_integer().width_type()) {
				// TODO: parse with the correct radix
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
	// we might be calling a built-in function
	if (std::holds_alternative<IR::BuiltInFunction>(function_call.callee))
		return call_built_in(std::get<IR::BuiltInFunction>(function_call.callee), function_call.arguments);
	AST::SymbolID callee_id = std::get<AST::SymbolID>(function_call.callee);
	// or even a symbol that points to a built-in function!
	if (std::holds_alternative<IR::BuiltInFunction>(symbols_.at(callee_id).item))
		return call_built_in(
			std::get<IR::BuiltInFunction>(symbols_.at(callee_id).item),
			function_call.arguments
		);
	// this should be an actual function now hopefully
	llvm::Function*           callee = program_.getFunction(get_name(callee_id));
	std::vector<llvm::Value*> arguments {};
	std::transform(
		function_call.arguments.cbegin(),
		function_call.arguments.cend(),
		std::back_inserter(arguments),
		[this](auto const& argument) { return generate_expression(argument.value); }
	);
	return builder_.CreateCall(callee, arguments);
}

llvm::Value* CodeGenerator::generate_expression(IR::Expression::Deref const& deref) {
	return builder_.CreateLoad(generate_type(deref.type), variables_[deref.address.value]);
}

llvm::Value* CodeGenerator::generate_expression(IR::Expression::Ref const& deref) {
	return variables_[deref.value.value];
}

llvm::Value* CodeGenerator::generate_expression(IR::Expression const& expression) {
	switch (expression.kind()) {
	case IR::Expression::Kind::Atom:         return generate_expression(expression.get_atom());
	case IR::Expression::Kind::FunctionCall: return generate_expression(expression.get_function_call());
	case IR::Expression::Kind::Deref:        return generate_expression(expression.get_deref());
	case IR::Expression::Kind::Ref:          return generate_expression(expression.get_ref());
	}
}

void CodeGenerator::emit_statement(IR::Statement::Declare const& declare, llvm::BasicBlock* block) {
	llvm::Type*  type  = generate_type(declare.type);
	llvm::Value* value = declare.value.has_value() ? generate_expression(declare.value.value().value) : nullptr;
	// FIXME: we should probably create this variable in the entry of the function
	variables_[declare.name.value]
		= llvm::IRBuilder<>(block, block->begin()).CreateAlloca(type, nullptr, get_name(declare.name.value));
	if (value) builder_.CreateStore(value, variables_[declare.name.value]);
}

void CodeGenerator::emit_statement(IR::Statement::Set const& set) {
	llvm::Value* value = generate_expression(set.value.value);
	builder_.CreateStore(value, variables_[set.name.value]);
}

void CodeGenerator::emit_statement(IR::Statement::Write const& write) {
	llvm::Value* value = generate_expression(write.value.value);
	builder_.CreateStore(value, builder_.CreateLoad(builder_.getPtrTy(), variables_[write.address.value]));
}

void CodeGenerator::emit_statement(IR::Statement const& statement, llvm::BasicBlock* block) {
	switch (statement.kind()) {
	case IR::Statement::Kind::Declare: return emit_statement(statement.get_declare(), block);
	case IR::Statement::Kind::Set:     return emit_statement(statement.get_set());
	case IR::Statement::Kind::Call:    generate_expression(statement.get_call()); return;
	case IR::Statement::Kind::Write:   return emit_statement(statement.get_write());
	}
}

void CodeGenerator::emit_basic_block(IR::BasicBlock const& basic_block, llvm::BasicBlock* block) {
	// this just emits the statements
	for (auto const& statement : basic_block.statements) emit_statement(statement.value, block);
}

void CodeGenerator::emit_basic_block_jump(
	IR::BasicBlock const&                                     basic_block,
	std::unordered_map<IR::BasicBlock::ID, llvm::BasicBlock*> blocks
) {
	if (std::holds_alternative<IR::BasicBlock::Goto>(basic_block.jump)) {
		IR::BasicBlock::Goto const& goto_ = std::get<IR::BasicBlock::Goto>(basic_block.jump);
		builder_.CreateBr(blocks.at(goto_.id));
	} else if (std::holds_alternative<IR::BasicBlock::Branch>(basic_block.jump)) {
		IR::BasicBlock::Branch const& branch    = std::get<IR::BasicBlock::Branch>(basic_block.jump);
		llvm::Value*                  condition = generate_expression(branch.condition);
		builder_.CreateCondBr(condition, blocks.at(branch.true_), blocks.at(branch.false_));
	} else if (std::holds_alternative<IR::BasicBlock::Return>(basic_block.jump)) {
		IR::BasicBlock::Return const& return_ = std::get<IR::BasicBlock::Return>(basic_block.jump);
		if (!return_.value.has_value()) {
			builder_.CreateRetVoid();
			return;
		}
		llvm::Value* value = generate_expression(return_.value.value().value);
		if (value->getType()->isVoidTy()) {
			builder_.CreateRetVoid();
			return;
		}
		builder_.CreateRet(value);
	} else {
		// this block is invalid, so we shouldn't have made it this far
		// let's add a return void for the lulz
		builder_.CreateRetVoid();
	}
}

void CodeGenerator::emit_function(IR::Function const& ir_function) {
	if (ir_function.body.empty()) return;
	llvm::Function* function = program_.getFunction(get_name(ir_function.name.value));
	assert(function);

	size_t count = 0;
	// we store two hashmaps to be able to add the jumps later on
	std::unordered_map<IR::BasicBlock::ID, IR::BasicBlock const*> basic_blocks {};
	std::unordered_map<IR::BasicBlock::ID, llvm::BasicBlock*>     blocks {};
	for (IR::BasicBlock const& basic_block : ir_function.body) {
		llvm::BasicBlock* block = llvm::BasicBlock::Create(context_, get_block_name(basic_block.id), function);
		builder_.SetInsertPoint(block);
		if (count++ == 0) {
			// if this is the first basic block, we have to insert argument boilerplate
			size_t i = 0;
			for (auto& arg : function->args()) {
				AST::SymbolID argument_id   = ir_function.arguments[i++].name.value;
				std::string   argument_name = get_name(argument_id);
				variables_[argument_id]     = llvm::IRBuilder<>(block, block->begin())
				                                  .CreateAlloca(arg.getType(), nullptr, argument_name);
				builder_.CreateStore(&arg, variables_[argument_id]);
			}
		}
		emit_basic_block(basic_block, block);
		blocks.emplace(basic_block.id, block);
		basic_blocks.emplace(basic_block.id, &basic_block);
	}

	// after emitting all statements, we can now emit all jumps
	for (auto const& [id, block] : blocks) {
		builder_.SetInsertPoint(block);
		emit_basic_block_jump(*basic_blocks.at(id), blocks);
	}

	llvm::verifyFunction(*function, &llvm::errs());
}

void CodeGenerator::create_function(IR::Function const& function) {
	std::vector<llvm::Type*> argument_types {};
	std::transform(
		function.arguments.cbegin(),
		function.arguments.cend(),
		std::back_inserter(argument_types),
		[this](auto const& argument) { return generate_type(argument.type.value); }
	);
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
		// however, we want the internal function to still exist, so we create a small stub
		llvm::Function* stub_function = llvm::Function::Create(
			function_type,
			llvm::Function::ExternalLinkage,
			get_name(function.name.value),
			&program_
		);
		// we need to make sure that if we're exporting a definition, the definition is on the correct one
		// though! that is, if we have a body, the exported one becomes the stub
		llvm::Function* defined = function.body.empty() ? actual_function : stub_function;
		llvm::Function* alias   = function.body.empty() ? stub_function : actual_function;
		// this stub just calls the function and returns whatever it returns
		llvm::BasicBlock* block = llvm::BasicBlock::Create(context_, "entry", alias);
		builder_.SetInsertPoint(block);
		std::vector<llvm::Value*> arguments {};
		arguments.reserve(alias->arg_size());
		std::transform(
			alias->args().begin(),
			alias->args().end(),
			std::back_inserter(arguments),
			[](auto& arg) { return &arg; }
		);
		llvm::Value* value = builder_.CreateCall(defined, arguments);
		if (value->getType()->isVoidTy()) builder_.CreateRetVoid();
		else builder_.CreateRet(value);
		// if we don't have a body, we want to actually replace all calls to the alias with calls to the
		// function
		if (function.body.empty()) alias->addFnAttr(llvm::Attribute::AlwaysInline);
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
