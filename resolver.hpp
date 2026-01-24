#pragma once
#include "ast/identifier.hpp"
#include "lexer.hpp"
#include "parser.hpp"

#include <unordered_map>

class Resolver {
public:
	struct ParsedFile {
		std::string source;
		std::vector<size_t> loc;
		AST::Module module;
	};

	std::vector<ParsedFile> parsed_files;

	explicit Resolver(std::vector<ParsedFile>&& parsed_files)
		: parsed_files(std::move(parsed_files))
		, module_table_ {}
		, counter_ {0} {}

	void resolve();

	inline std::vector<Diagnostic> const& diagnostics() const { return diagnostics_; }

private:
	std::unordered_map<std::string, AST::Module*> module_table_;

	// FIXME: diagnostics should be file-specific. fixable via file-specific spans.
	std::vector<Diagnostic> diagnostics_;

	uint32_t counter_;

	/// Returns a new ID produced by the counter.
	uint32_t next();

	/// Populates the module table according to the parsed files.
	void populate_module_table();

	/// Adds an ID to the identifier.
	void identify(AST::Identifier&);
	/// Identifies the module with an ID and its non-import items.
	void identify(AST::Module&);
	/// Identifies the function with an ID.
	void identify(AST::Function&);
	/// Identifies all module items with an ID, but does not resolve imports.
	void identify_module_items();

	void resolve(AST::Module&);
	/// Specifically resolves imports.
	void resolve_imports();
	/// Resolves function bodies and all remaining identifiers.
	void resolve_identifiers();
};
