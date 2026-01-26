#pragma once
#include "ast/function.hpp"
#include "ast/identifier.hpp"
#include "ast/module.hpp"
#include "lexer.hpp"

#include <unordered_map>

class Resolver {
public:
	struct ParsedFile {
		// TODO: make this file_id stuff more consistent (it matches with the idx in parsed_files, but that's
		// not quite evident as of right now)
		uint32_t                file_id;
		std::string             source;
		std::string             name;
		std::vector<size_t>     loc;
		AST::Module             module;
		std::vector<Diagnostic> diagnostics;
	};

	std::vector<ParsedFile> parsed_files;

	explicit Resolver(std::vector<ParsedFile>&& parsed_files)
		: parsed_files(std::move(parsed_files))
		, module_table_ {}
		, counter_ {0} {}

	void resolve();

private:
	struct Symbol {
		uint32_t                                                   id;
		uint32_t                                                   file_id;
		Span                                                       span;
		std::string                                                name;
		std::variant<AST::Module*, AST::Function*, std::monostate> item;
		bool                                                       mutable_;
	};

	std::vector<Symbol> symbol_pool_;

	inline Symbol& get_single_symbol(uint32_t id) { return symbol_pool_.at(id); }

	inline Symbol& get_single_symbol(AST::Identifier const& identifier) {
		std::cout << identifier << std::endl;
		assert(identifier.id.has_value() && identifier.id.value().size() == 1);
		return get_single_symbol(identifier.id.value().at(0));
	}

	struct Scope {
		Scope const* parent = nullptr;
		std::unordered_map<std::string, std::vector<Symbol*>> symbols;
	};

	std::unordered_map<std::string, AST::Module*> module_table_;

	uint32_t counter_;

	/// Returns a new ID produced by the counter.
	uint32_t next();

	/// Returns the file context for the requested file ID.
	FileContext get_context(uint32_t file_id);

	/// Populates the module table according to the parsed files.
	void populate_module_table();

	/// Produces a single ID for the identifier and sets it; the caller must register this ID in the symbol pool.
	void identify(AST::Identifier&);
	/// Identifies the module with an ID and its non-import items.
	void identify(AST::Module&, uint32_t file_id);
	/// Identifies the function with an ID.
	void identify(AST::Function&, uint32_t file_id);
	/// Identifies all module items with an ID, but does not resolve imports.
	void identify_module_items();

	// TODO: return the ID for ergonomics in some of these
	void resolve(Spanned<AST::Identifier>&, Scope const&, uint32_t file_id);
	void resolve(AST::Expression::UnaryOperation&, Scope const&, uint32_t file_id);
	void resolve(AST::Expression::BinaryOperation&, Scope const&, uint32_t file_id);
	void resolve(AST::Expression::FunctionCall&, Scope const&, uint32_t file_id);
	void resolve(AST::Expression&, Span, Scope const&, uint32_t file_id);
	void resolve(Spanned<AST::Expression>&, Scope const&, uint32_t file_id);
	void resolve(AST::Statement::Declare&, Scope&, uint32_t file_id);
	void resolve(AST::Statement::Set&, Scope&, uint32_t file_id);
	void resolve(AST::Statement::Return&, Scope&, uint32_t file_id);
	void resolve(Spanned<AST::Statement>&, Scope&, uint32_t file_id);
	void resolve(AST::Scope&, Scope, uint32_t file_id);
	void resolve(AST::Function&, Scope, uint32_t file_id);
	void resolve(AST::Module&, Scope, uint32_t file_id);
	/// Resolves function bodies and, as such, all identifiers within.
	void resolve_identifiers();
};
