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
		FileContext::ID         file_id;
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
		AST::SymbolID   id;
		FileContext::ID file_id;
		Span            span;
		std::string     name;

		std::variant<AST::Module*, AST::Function*, std::monostate> item;

		bool mutable_;
	};

	std::vector<Symbol> symbol_pool_;

	inline Symbol& get_single_symbol(AST::SymbolID id) { return symbol_pool_.at(id); }

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

	AST::SymbolID counter_;

	/// Returns a new ID produced by the counter.
	AST::SymbolID next();

	/// Returns the file context for the requested file ID.
	FileContext get_context(FileContext::ID);

	/// Populates the module table according to the parsed files.
	void populate_module_table();

	/// Produces a single ID for the identifier and sets it; the caller must register this ID in the symbol pool.
	void identify(AST::Identifier&);
	/// Identifies the module with an ID and its non-import items.
	void identify(AST::Module&, FileContext::ID);
	/// Identifies the function with an ID.
	void identify(AST::Function&, FileContext::ID);
	/// Identifies all module items with an ID, but does not resolve imports.
	void identify_module_items();

	// TODO: return the ID for ergonomics in some of these
	void resolve(Spanned<AST::Identifier>&, Scope const&, FileContext::ID);
	void resolve(AST::Expression::UnaryOperation&, Scope const&, FileContext::ID);
	void resolve(AST::Expression::BinaryOperation&, Scope const&, FileContext::ID);
	void resolve(AST::Expression::FunctionCall&, Scope const&, FileContext::ID);
	void resolve(AST::Expression&, Span, Scope const&, FileContext::ID);
	void resolve(Spanned<AST::Expression>&, Scope const&, FileContext::ID);
	void resolve(AST::Statement::Declare&, Scope&, FileContext::ID);
	void resolve(AST::Statement::Set&, Scope&, FileContext::ID);
	void resolve(AST::Statement::Return&, Scope&, FileContext::ID);
	void resolve(Spanned<AST::Statement>&, Scope&, FileContext::ID);
	void resolve(AST::Scope&, Scope, FileContext::ID);
	void resolve(AST::Function&, Scope, FileContext::ID);
	void resolve(AST::Module&, Scope, FileContext::ID);
	/// Resolves function bodies and, as such, all identifiers within.
	void resolve_identifiers();
};
