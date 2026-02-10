#pragma once
#include "ir.hpp"

#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Type.h>
#include <unordered_map>

class CodeGenerator {
public:
	explicit CodeGenerator(std::vector<IR::Symbol>&& symbols)
		: context_()
		, program_("program", context_)
		, builder_(context_)
		, symbols_(std::move(symbols)) {}

	enum class Optimization { O0, O1, O2, O3 };

	void process(std::vector<IR::Module> const&, std::string of, Optimization);

private:
	llvm::LLVMContext context_;
	// TODO: separate files into several llvm modules, which will require imports to be stored in the per-file IR
	llvm::Module      program_;
	llvm::IRBuilder<> builder_;

	std::unordered_map<IR::Identifier, llvm::Value*> variables_;

	std::vector<IR::Symbol> symbols_;

	std::string get_name(IR::Identifier) const;
	std::string get_block_name(IR::BasicBlock::ID) const;

	llvm::Type* generate_type(IR::Type::Atom const&);
	llvm::Type* generate_type(IR::Type const&);

	llvm::Value* call_built_in(IR::BuiltInFunction, std::vector<Spanned<IR::Expression::Atom>> const& arguments);

	llvm::Value* generate_expression(IR::Expression::Atom const&);
	llvm::Value* generate_expression(IR::Expression::FunctionCall const&);
	llvm::Value* generate_expression(IR::Expression::Deref const&);
	llvm::Value* generate_expression(IR::Expression::Ref const&);
	llvm::Value* generate_expression(IR::Expression const&);

	void emit_statement(IR::Statement::Declare const&, llvm::BasicBlock*);
	void emit_statement(IR::Statement::Set const&);
	void emit_statement(IR::Statement::Write const&);
	void emit_statement(IR::Statement const&, llvm::BasicBlock*);

	void emit_basic_block(IR::BasicBlock const&, llvm::BasicBlock*);
	void emit_basic_block_jump(IR::BasicBlock const&, std::unordered_map<IR::BasicBlock::ID, llvm::BasicBlock*>);

	/// Assumes the function has already been created.
	void emit_function(IR::Function const&);

	void create_function(IR::Function const&);

	void create_all_functions(IR::Module const&);
	void create_all_functions(std::vector<IR::Module> const&);

	void emit_all_functions(IR::Module const&);
	void emit_all_functions(std::vector<IR::Module> const&);
};
