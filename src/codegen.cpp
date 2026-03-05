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
	create_all(modules);
	emit_remaining();

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
		llvm::errs() << "fatal error: could not open file: " << ec.message();
		std::exit(1);
	}

	llvm::legacy::PassManager pass;

	auto file_type = llvm::CodeGenFileType::ObjectFile;

	if (target_machine->addPassesToEmitFile(pass, dest, nullptr, file_type)) {
		llvm::errs() << "fatal error: TargetMachine can't emit a file of this type";
		std::exit(1);
	}

	pass.run(program_);
	dest.flush();
}

std::string CodeGenerator::get_name(IR::Identifier identifier) const {
	return std::format("{}_{}", symbols_.at(identifier).name, identifier);
}

std::string CodeGenerator::get_name_generics(IR::Identifier name, IR::GenericList const& generic_list) const {
	size_t      instantiation_idx = SIZE_MAX;
	auto const& instantiations    = symbols_.at(name).instantiations;
	for (size_t i = 0; i < instantiations.size(); ++i) {
		if (instantiations.at(i) == generic_list) {
			instantiation_idx = i;
			break;
		}
	}
	assert(instantiation_idx != SIZE_MAX && "we missed an instantiation somehow!");
	return std::format("{}_{}i{}", symbols_.at(name).name, name, instantiation_idx);
}

std::string CodeGenerator::get_block_name(IR::BasicBlock::ID id) const {
	return std::format("_{}b", id);
}

bool CodeGenerator::has_instantiation(IR::Identifier name, IR::GenericList const& generic_list) {
	auto const& instantiations = symbols_.at(name).instantiations;
	return std::find(instantiations.cbegin(), instantiations.cend(), generic_list) != instantiations.cend();
}

void CodeGenerator::add_instantiation(IR::Identifier name, IR::GenericList const& generic_list) {
	assert(!has_instantiation(name, generic_list));
	symbols_.at(name).instantiations.push_back(clone(generic_list));
}

CodeGenerator::GenericCtx CodeGenerator::create_generic_ctx(
	IR::GenericDeclaration const& generic_declaration,
	IR::GenericList const&        generic_list
) {
	GenericCtx generic_ctx {};
	assert(generic_declaration.size() == generic_list.size());
	for (size_t i = 0; i < generic_declaration.size(); ++i) {
		auto name = generic_declaration.at(i).name;
		assert(!generic_ctx.contains(name));
		auto type = generic_list.at(i).value.clone();
		generic_ctx.insert_or_assign(name, std::move(type));
	}
	return generic_ctx;
}

IR::Type CodeGenerator::apply_generic_ctx(IR::Type const& type, GenericCtx const& generic_ctx) {
	switch (type.kind()) {
	case IR::Type::Kind::Atom: break;
	case IR::Type::Kind::Pointer:
		return IR::Type::make_pointer(
			IR::Type::Pointer {
				std::make_unique<Spanned<IR::Type>>(
					type.get_pointer().type->span,
					apply_generic_ctx(type.get_pointer().type->value, generic_ctx)
				),
				type.get_pointer().mutable_
			}
		);
	}

	if (!type.get_atom().is_named()) return type.clone();
	if (generic_ctx.contains(type.get_atom().get_named().name.value))
		return apply_generic_ctx(generic_ctx.at(type.get_atom().get_named().name.value), generic_ctx);
	return type.clone();
}

llvm::Type* CodeGenerator::get_struct_type(IR::Type::Atom::Named const& named, GenericCtx const& generic_ctx) {
	IR::GenericList actual_generic_list {};
	actual_generic_list.reserve(named.generic_list.size());
	std::transform(
		named.generic_list.cbegin(),
		named.generic_list.cend(),
		std::back_inserter(actual_generic_list),
		[this, &generic_ctx](Spanned<IR::Type> const& type) {
			return Spanned {type.span, apply_generic_ctx(type.value, generic_ctx)};
		}
	);
	create_struct(
		std::get<IR::Struct>(symbols_.at(named.name.value).item),
		named.name.value,
		named.generic_list,
		generic_ctx
	);
	return llvm::StructType::getTypeByName(context_, get_name_generics(named.name.value, named.generic_list));
}

llvm::Type* CodeGenerator::generate_type(IR::Type::Atom const& atom, GenericCtx const& generic_ctx) {
	std::cout << "[codegen] generating type atom " << atom << "\n";
	std::cout << "\tgeneric ctx:\n";
	for (auto const& [a, b] : generic_ctx) {
		std::cout << "\t\t@" << a << " -> " << b << "\n";
	}
	std::cout.flush();

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
	case IR::Type::Atom::Kind::Named: break;
	}

	if (generic_ctx.contains(atom.get_named().name.value))
		return generate_type(generic_ctx.at(atom.get_named().name.value), generic_ctx);
	return get_struct_type(atom.get_named(), generic_ctx);
}

llvm::Type* CodeGenerator::generate_type(IR::Type const& type, GenericCtx const& generic_ctx) {
	switch (type.kind()) {
	case IR::Type::Kind::Atom:    return generate_type(type.get_atom(), generic_ctx);
	case IR::Type::Kind::Pointer: return builder_.getPtrTy();
	}
}

llvm::Value* CodeGenerator::call_built_in(
	IR::BuiltInFunction                          function,
	GenericCtx const&                            generic_ctx,
	std::vector<Spanned<IR::Value::Atom>> const& function_arguments
) {
	std::vector<llvm::Value*> arguments {};
	std::transform(
		function_arguments.cbegin(),
		function_arguments.cend(),
		std::back_inserter(arguments),
		[this, &generic_ctx](auto const& argument) { return generate_value(argument.value, generic_ctx); }
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

llvm::Value* CodeGenerator::get_place_pointer(IR::Place const& place, GenericCtx const& generic_ctx) {
	// derefs don't actually deref because pointers are pointers seemingly
	switch (place.kind()) {
	case IR::Place::Kind::Symbol: return variables_[place.get_symbol()];
	case IR::Place::Kind::Deref:  return get_place_pointer(place.get_deref().address->value, generic_ctx);
	case IR::Place::Kind::Access:
		return builder_.CreateStructGEP(
			generate_type(place.get_access().accessee->value.type, generic_ctx),
			get_place_pointer(place.get_access().accessee->value, generic_ctx),
			place.get_access().field_index
		);
	case IR::Place::Kind::Error: [[assume(false)]];
	}
}

llvm::Value* CodeGenerator::generate_value(IR::Value::Atom const& atom, GenericCtx const& generic_ctx) {
	switch (atom.kind()) {
	case IR::Value::Atom::Kind::Identifier:
		return builder_.CreateLoad(generate_type(atom.type, generic_ctx), variables_[atom.get_identifier()]);
	case IR::Value::Atom::Kind::Literal:       break;
	case IR::Value::Atom::Kind::Bool:          return builder_.getInt1(atom.get_bool());
	case IR::Value::Atom::Kind::StructLiteral: break;
	case IR::Value::Atom::Kind::Error:         [[assume(false)]];
	}

	if (atom.is_struct_literal()) {
		std::vector<llvm::Value*> fields {};
		fields.reserve(atom.get_struct_literal().fields.size());
		std::transform(
			atom.get_struct_literal().fields.cbegin(),
			atom.get_struct_literal().fields.cend(),
			std::back_inserter(fields),
			[this, &generic_ctx](Spanned<IR::Value::Atom> const& value) {
				return generate_value(value.value, generic_ctx);
			}
		);
		auto type = get_struct_type(atom.get_struct_literal().type, generic_ctx);

		// we have to manually create it this way because we don't have a guarantee that the values are constant
		llvm::Value* value = llvm::UndefValue::get(type);
		for (size_t i = 0; i < fields.size(); ++i) {
			value = builder_.CreateInsertValue(value, fields[i], {static_cast<uint32_t>(i)});
		}
		return value;
	}

	// literals vary depending on their type
	IR::Value::Atom::Literal const& literal = atom.get_literal();

	switch (literal.kind) {
	case IR::Value::Atom::Literal::Kind::Number:
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
		case IR::Type::Atom::Kind::Named:
		case IR::Type::Atom::Kind::Error:
			std::cout << "genuinely what are you doing" << std::endl;
			std::exit(0);
		}

	case IR::Value::Atom::Literal::Kind::String:
		std::cout << "unsupported string literal lol!!" << std::endl;
		std::exit(0);
	case IR::Value::Atom::Literal::Kind::Char: break;
	}

	// for chars, we need to look up the codepoint
	// FIXME: this is very primitive
	uint8_t codepoint = 0;
	if (literal.literal.size() == 3) codepoint = literal.literal[1];
	else if (literal.literal.size() == 4) codepoint = Lexer::lookup_escaped(literal.literal[2]).value();
	return builder_.getInt8(codepoint);
}

llvm::Value*
CodeGenerator::generate_value(IR::Value::FunctionCall const& function_call, GenericCtx const& generic_ctx) {
	// we might be calling a built-in function
	if (std::holds_alternative<IR::BuiltInFunction>(function_call.callee))
		return call_built_in(
			std::get<IR::BuiltInFunction>(function_call.callee),
			generic_ctx,
			function_call.arguments
		);
	IR::Identifier callee_id = std::get<Spanned<IR::Identifier>>(function_call.callee).value;
	// or even a symbol that points to a built-in function!
	if (std::holds_alternative<IR::BuiltInFunction>(symbols_.at(callee_id).item))
		return call_built_in(
			std::get<IR::BuiltInFunction>(symbols_.at(callee_id).item),
			generic_ctx,
			function_call.arguments
		);
	// this should be an actual function now hopefully
	// ensure the function is created first
	create_function(
		std::get<IR::Function>(symbols_.at(callee_id).item),
		callee_id,
		function_call.generic_list,
		generic_ctx
	);
	llvm::Function* callee = program_.getFunction(get_name_generics(callee_id, function_call.generic_list));
	std::vector<llvm::Value*> arguments {};
	std::transform(
		function_call.arguments.cbegin(),
		function_call.arguments.cend(),
		std::back_inserter(arguments),
		[this, &generic_ctx](auto const& argument) { return generate_value(argument.value, generic_ctx); }
	);
	return builder_.CreateCall(callee, arguments);
}

llvm::Value* CodeGenerator::generate_value(IR::Value::Ref const& ref, GenericCtx const& generic_ctx) {
	return get_place_pointer(ref.value.value, generic_ctx);
}

llvm::Value* CodeGenerator::generate_value(IR::Value::Load const& load, GenericCtx const& generic_ctx) {
	return builder_.CreateLoad(
		generate_type(load.value.value.type, generic_ctx),
		get_place_pointer(load.value.value, generic_ctx)
	);
}

llvm::Value* CodeGenerator::generate_value(IR::Value const& value, GenericCtx const& generic_ctx) {
	switch (value.kind()) {
	case IR::Value::Kind::Atom:         return generate_value(value.get_atom(), generic_ctx);
	case IR::Value::Kind::FunctionCall: return generate_value(value.get_function_call(), generic_ctx);
	case IR::Value::Kind::Ref:          return generate_value(value.get_ref(), generic_ctx);
	case IR::Value::Kind::Load:         return generate_value(value.get_load(), generic_ctx);
	}
}

void CodeGenerator::emit_statement(
	IR::Statement::Declare const& declare,
	GenericCtx const&             generic_ctx,
	llvm::BasicBlock*             block
) {
	llvm::Type* type = generate_type(declare.type, generic_ctx);
	// FIXME: we should probably create this variable in the entry of the function
	variables_[declare.name.value]
		= llvm::IRBuilder<>(block, block->begin()).CreateAlloca(type, nullptr, get_name(declare.name.value));
}

void CodeGenerator::emit_statement(IR::Statement::Set const& set, GenericCtx const& generic_ctx) {
	llvm::Value* value = generate_value(set.value.value, generic_ctx);
	builder_.CreateStore(value, get_place_pointer(set.place.value, generic_ctx));
}

void CodeGenerator::emit_statement(
	IR::Statement const& statement,
	GenericCtx const&    generic_ctx,
	llvm::BasicBlock*    block
) {
	switch (statement.kind()) {
	case IR::Statement::Kind::Declare: return emit_statement(statement.get_declare(), generic_ctx, block);
	case IR::Statement::Kind::Set:     return emit_statement(statement.get_set(), generic_ctx);
	case IR::Statement::Kind::Call:    generate_value(statement.get_call(), generic_ctx); return;
	}
}

void CodeGenerator::emit_basic_block(
	IR::BasicBlock const& basic_block,
	GenericCtx const&     generic_ctx,
	llvm::BasicBlock*     block
) {
	// this just emits the statements
	for (auto const& statement : basic_block.statements) emit_statement(statement.value, generic_ctx, block);
}

void CodeGenerator::emit_basic_block_jump(
	IR::BasicBlock const&                                     basic_block,
	GenericCtx const&                                         generic_ctx,
	std::unordered_map<IR::BasicBlock::ID, llvm::BasicBlock*> blocks
) {
	if (std::holds_alternative<IR::BasicBlock::Goto>(basic_block.jump)) {
		IR::BasicBlock::Goto const& goto_ = std::get<IR::BasicBlock::Goto>(basic_block.jump);
		builder_.CreateBr(blocks.at(goto_.id));
	} else if (std::holds_alternative<IR::BasicBlock::Branch>(basic_block.jump)) {
		IR::BasicBlock::Branch const& branch    = std::get<IR::BasicBlock::Branch>(basic_block.jump);
		llvm::Value*                  condition = generate_value(branch.condition.value, generic_ctx);
		builder_.CreateCondBr(condition, blocks.at(branch.true_), blocks.at(branch.false_));
	} else if (std::holds_alternative<IR::BasicBlock::Return>(basic_block.jump)) {
		IR::BasicBlock::Return const& return_ = std::get<IR::BasicBlock::Return>(basic_block.jump);
		if (!return_.value.has_value()) {
			builder_.CreateRetVoid();
			return;
		}
		llvm::Value* value = generate_value(return_.value.value().value, generic_ctx);
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

void CodeGenerator::emit_function(
	IR::Function const&    ir_function,
	IR::GenericList const& generic_list,
	GenericCtx const&      generic_ctx
) {
	if (ir_function.body.empty()) return;
	llvm::Function* function = program_.getFunction(get_name_generics(ir_function.name.value, generic_list));
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
				IR::Identifier argument_id   = ir_function.arguments[i++].name.value;
				std::string    argument_name = get_name(argument_id);
				variables_[argument_id]      = llvm::IRBuilder<>(block, block->begin())
				                                  .CreateAlloca(arg.getType(), nullptr, argument_name);
				builder_.CreateStore(&arg, variables_[argument_id]);
			}
		}
		emit_basic_block(basic_block, generic_ctx, block);
		blocks.emplace(basic_block.id, block);
		basic_blocks.emplace(basic_block.id, &basic_block);
	}

	// after emitting all statements, we can now emit all jumps
	for (auto const& [id, block] : blocks) {
		builder_.SetInsertPoint(block);
		emit_basic_block_jump(*basic_blocks.at(id), generic_ctx, blocks);
	}

	llvm::verifyFunction(*function, &llvm::errs());
}

void CodeGenerator::emit_struct(
	IR::Struct const&      struct_,
	IR::GenericList const& generic_list,
	GenericCtx const&      generic_ctx
) {
	llvm::StructType* struct_type
		= llvm::StructType::getTypeByName(context_, get_name_generics(struct_.name.value, generic_list));

	std::vector<llvm::Type*> field_types {};
	field_types.reserve(struct_.fields.size());
	std::transform(
		struct_.fields.cbegin(),
		struct_.fields.cend(),
		std::back_inserter(field_types),
		[this, &generic_ctx](IR::Struct::Field const& field) {
			return generate_type(field.type.value, generic_ctx);
		}
	);

	struct_type->setBody(field_types);
}

void CodeGenerator::create_function(
	IR::Function const&    function,
	IR::Identifier         name,
	IR::GenericList const& generic_list,
	GenericCtx const&      inherit
) {
	if (has_instantiation(name, generic_list)) return;
	add_instantiation(name, generic_list);

	GenericCtx generic_ctx = create_generic_ctx(function.generic_declaration, generic_list);
	for (auto const& [type_name, type] : inherit) { generic_ctx.insert_or_assign(type_name, type.clone()); }

	std::vector<llvm::Type*> argument_types {};
	std::transform(
		function.arguments.cbegin(),
		function.arguments.cend(),
		std::back_inserter(argument_types),
		[this, &generic_ctx](auto const& argument) { return generate_type(argument.type.value, generic_ctx); }
	);
	llvm::Type*         return_type   = generate_type(function.return_type.value, generic_ctx);
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
			get_name_generics(function.name.value, generic_list),
			&program_
		);
		// we need to make sure that if we're exporting a definition, the definition is on the correct one
		// though! that is, if we have a body, the exported one becomes the stub
		llvm::Function* defined = function.body.empty() ? actual_function : stub_function;
		llvm::Function* alias   = function.body.empty() ? stub_function : actual_function;
		emission_queue.push(Emission {&function, clone(generic_list), std::move(generic_ctx)});
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
		get_name_generics(function.name.value, generic_list),
		&program_
	);

	emission_queue.push(Emission {&function, clone(generic_list), std::move(generic_ctx)});
}

void CodeGenerator::create_struct(
	IR::Struct const&      struct_,
	IR::Identifier         name,
	IR::GenericList const& generic_list,
	GenericCtx const&      inherit
) {
	if (has_instantiation(name, generic_list)) return;
	add_instantiation(name, generic_list);

	GenericCtx generic_ctx = create_generic_ctx(struct_.generic_declaration, generic_list);
	for (auto const& [type_name, type] : inherit) { generic_ctx.insert_or_assign(type_name, type.clone()); }

	llvm::StructType::create(context_, get_name_generics(struct_.name.value, generic_list));

	emit_struct(struct_, generic_list, generic_ctx);
}

void CodeGenerator::create_all(IR::Module const& module) {
	for (IR::Identifier item : module.items) {
		if (std::holds_alternative<IR::Module>(symbols_.at(item).item))
			create_all(std::get<IR::Module>(symbols_.at(item).item));
		else if (std::holds_alternative<IR::Function>(symbols_.at(item).item)) {
			auto& function = std::get<IR::Function>(symbols_.at(item).item);
			// we skip functions with generics, we instantiate them later
			if (!function.generic_declaration.empty()) continue;
			create_function(function, item, {}, {});
		} else if (std::holds_alternative<IR::Struct>(symbols_.at(item).item)) {
			auto& struct_ = std::get<IR::Struct>(symbols_.at(item).item);
			// we skip structs with generics, we instatiate them later
			if (!struct_.generic_declaration.empty()) continue;
			create_struct(struct_, item, {}, {});
		}
	}
}

void CodeGenerator::create_all(std::vector<IR::Module> const& modules) {
	for (IR::Module const& module : modules) { create_all(module); }
}

void CodeGenerator::emit_remaining() {
	while (!emission_queue.empty()) {
		auto emission = std::move(emission_queue.front());
		emission_queue.pop();
		emit_function(*emission.what, emission.generic_list, emission.generic_ctx);
	}
}
