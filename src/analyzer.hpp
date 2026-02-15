#include "ir.hpp"

#include <unordered_map>
#include <vector>

class Analyzer {
public:
	struct ResolvedFile {
		FileContext::ID         file_id;
		std::string             source;
		std::string             name;
		std::vector<size_t>     loc;
		IR::Module              module;
		std::vector<Diagnostic> diagnostics;
	};

	std::vector<ResolvedFile> resolved_files;
	std::vector<IR::Symbol>   symbols;

	explicit Analyzer(std::vector<IR::Symbol>&& symbols, std::vector<ResolvedFile>&& resolved_files)
		: resolved_files(std::move(resolved_files))
		, symbols(std::move(symbols)) {}

	void analyze();

private:
	void check_assigned(IR::Statement&, std::unordered_map<AST::SymbolID, bool>& mutability_map);
	void check_assigned(IR::BasicBlock&, std::unordered_map<AST::SymbolID, bool>& mutability_map);
	void check_assigned(IR::Function&);
	void check_assigned(IR::Module&);
	/// Checks whether each value has been assigned once before being used. Also checks mutability for set
	/// statements, allowing constant variables to be set once if they were originally set to be undefined.
	void check_assigned();
};
