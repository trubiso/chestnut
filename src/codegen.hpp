#pragma once
#include "ir.hpp"

#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Type.h>
#include <queue>
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

	using GenericCtx = std::unordered_map<IR::Identifier, IR::Type>;

	std::string get_name(IR::Identifier) const;
	std::string get_name_generics(IR::Identifier, IR::GenericList const&) const;
	std::string get_block_name(IR::BasicBlock::ID) const;

	bool has_instantiation(IR::Identifier, IR::GenericList const&);
	void add_instantiation(IR::Identifier, IR::GenericList const&);

	GenericCtx create_generic_ctx(IR::GenericDeclaration const&, IR::GenericList const&);

	llvm::Type* get_struct_type(IR::Type::Atom::Named const&);

	llvm::Type* generate_type(IR::Type::Atom const&, GenericCtx const&);
	llvm::Type* generate_type(IR::Type const&, GenericCtx const&);

	llvm::Value*
	call_built_in(IR::BuiltInFunction, GenericCtx const&, std::vector<Spanned<IR::Value::Atom>> const& arguments);

	llvm::Value* get_place_pointer(IR::Place const&, GenericCtx const&);

	llvm::Value* generate_value(IR::Value::Atom const&, GenericCtx const&);
	llvm::Value* generate_value(IR::Value::FunctionCall const&, GenericCtx const&);
	llvm::Value* generate_value(IR::Value::Ref const&, GenericCtx const&);
	llvm::Value* generate_value(IR::Value::Load const&, GenericCtx const&);
	llvm::Value* generate_value(IR::Value const&, GenericCtx const&);

	void emit_statement(IR::Statement::Declare const&, GenericCtx const&, llvm::BasicBlock*);
	void emit_statement(IR::Statement::Set const&, GenericCtx const&);
	void emit_statement(IR::Statement const&, GenericCtx const&, llvm::BasicBlock*);

	void emit_basic_block(IR::BasicBlock const&, GenericCtx const&, llvm::BasicBlock*);
	void emit_basic_block_jump(
		IR::BasicBlock const&,
		GenericCtx const&,
		std::unordered_map<IR::BasicBlock::ID, llvm::BasicBlock*>
	);

	/// Assumes the function has already been created.
	void emit_function(IR::Function const&, IR::GenericList const&, GenericCtx const&);
	/// Assumes the struct has already been created.
	void emit_struct(IR::Struct const&, IR::GenericList const&, GenericCtx const&);

	struct Emission {
		std::variant<IR::Function const*, IR::Struct const*> what;
		IR::GenericList                                      generic_list;
		GenericCtx                                           generic_ctx;
	};

	std::queue<Emission> emission_queue;

	void create_function(IR::Function const&, IR::Identifier, IR::GenericList const&);
	void create_struct(IR::Struct const&, IR::Identifier, IR::GenericList const&);

	void create_all(IR::Module const&);
	void create_all(std::vector<IR::Module> const&);

	void emit_remaining();
};
