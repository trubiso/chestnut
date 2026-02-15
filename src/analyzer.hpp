#include "ir.hpp"

#include <unordered_map>
#include <unordered_set>
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
	/// Returns the file context for the requested file ID.
	FileContext get_context(FileContext::ID) const;

	void check_assigned(
		IR::Identifier,
		Span,
		FileContext::ID,
		std::unordered_map<IR::Identifier, bool> const& assigned
	);
	void check_assigned(
		Spanned<IR::Identifier> const&,
		FileContext::ID,
		std::unordered_map<IR::Identifier, bool> const& assigned
	);
	void check_assigned(
		IR::Expression::Atom const&,
		Span,
		FileContext::ID,
		std::unordered_map<IR::Identifier, bool> const& assigned
	);
	void check_assigned(
		IR::Expression::FunctionCall const&,
		FileContext::ID,
		std::unordered_map<IR::Identifier, bool> const& assigned
	);
	void check_assigned(
		IR::Expression::Deref const&,
		FileContext::ID,
		std::unordered_map<IR::Identifier, bool> const& assigned
	);
	void check_assigned(
		IR::Expression::Ref const&,
		FileContext::ID,
		std::unordered_map<IR::Identifier, bool> const& assigned
	);
	void check_assigned(
		IR::Expression::MemberAccess const&,
		FileContext::ID,
		std::unordered_map<IR::Identifier, bool> const& assigned
	);
	void check_assigned(
		Spanned<IR::Expression::Atom> const&,
		FileContext::ID,
		std::unordered_map<IR::Identifier, bool> const& assigned
	);
	void check_assigned(
		Spanned<IR::Expression> const&,
		FileContext::ID,
		std::unordered_map<IR::Identifier, bool> const& assigned
	);
	void check_assigned(
		IR::Statement::Declare&,
		FileContext::ID,
		std::unordered_map<IR::Identifier, bool>& assigned
	);
	void check_assigned(IR::Statement::Set&, FileContext::ID, std::unordered_map<IR::Identifier, bool>& assigned);
	void check_assigned(IR::Statement::Write&, FileContext::ID, std::unordered_map<IR::Identifier, bool>& assigned);
	void check_assigned(
		IR::Statement::WriteAccess&,
		FileContext::ID,
		std::unordered_map<IR::Identifier, bool>& assigned
	);
	void check_assigned(IR::Statement&, FileContext::ID, std::unordered_map<IR::Identifier, bool>& assigned);
	void check_assigned(
		IR::BasicBlock&,
		IR::Function&,
		FileContext::ID,
		std::unordered_map<IR::Identifier, bool>&                                       assigned,
		std::unordered_map<IR::BasicBlock::ID, std::unordered_set<IR::BasicBlock::ID>>& preds
	);
	void check_assigned(IR::Function&, FileContext::ID);
	void check_assigned(IR::Module&, FileContext::ID);
	/// Checks whether each value has been assigned once before being used. Also checks mutability for set
	/// statements, allowing constant variables to be set once if they were originally set to be undefined.
	// TODO: also check mutability and kill two birds with one stone
	void check_assigned();
};
