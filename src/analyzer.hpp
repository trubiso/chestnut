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

	void analyze(bool print_ir = false);
	/// Prints an IR module using the symbol table.
	std::ostream& print(std::ostream&, IR::Module const&) const;

private:
	/// Returns the file context for the requested file ID.
	FileContext get_context(FileContext::ID) const;

	void optimize_blocks(IR::Function&);
	void optimize_blocks(IR::Module&);
	/// Optimizes away blocks which are unconditional jumps, and optimizes away unconditional jumps to blocks which
	/// are empty.
	void optimize_blocks();

	using PredsMap    = std::unordered_map<IR::BasicBlock::ID, std::unordered_set<IR::BasicBlock::ID>>;
	using AssignedMap = std::unordered_map<IR::Identifier, std::optional<Span>>;
	using MovedMap    = std::vector<std::tuple<IR::Place, Span>>;

	std::optional<IR::Identifier> get_base(IR::Place const&);

	bool get_mutable(IR::Place const&);

	Spanned<IR::Place> const* get_immutability_culprit(Spanned<IR::Place> const&);

	struct MoveInfo {
		std::vector<Span> span;
		bool              partial;
	};

	std::optional<bool>     check_moved(IR::Place const& checked, IR::Place const&);
	std::optional<MoveInfo> check_moved(IR::Place const&, MovedMap const&);

	void undo_move(IR::Place const&, MovedMap&);
	void do_move(IR::Place const&, Span, FileContext::ID, MovedMap&);

	void check_assigned(IR::Identifier, Span, FileContext::ID, AssignedMap const&, MovedMap&, bool moves);
	void check_assigned(Spanned<IR::Identifier> const&, FileContext::ID, AssignedMap const&, MovedMap&);
	void check_assigned(Spanned<IR::Place> const&, FileContext::ID, AssignedMap const&, MovedMap&, bool moves);

	void check_assigned(IR::Value::Atom const&, Span, FileContext::ID, AssignedMap const&, MovedMap&);
	void check_assigned(IR::Value::FunctionCall const&, FileContext::ID, AssignedMap const&, MovedMap&);
	void check_assigned(IR::Value::Ref const&, Span, FileContext::ID, AssignedMap const&, MovedMap&);
	void check_assigned(Spanned<IR::Value::Atom> const&, FileContext::ID, AssignedMap const&, MovedMap&);
	void check_assigned(Spanned<IR::Value> const&, FileContext::ID, AssignedMap const&, MovedMap&);

	void check_assigned(IR::Statement::Declare&, FileContext::ID, AssignedMap&, MovedMap&);
	void check_assigned(IR::Statement::Set&, FileContext::ID, AssignedMap&, MovedMap&);
	void check_assigned(IR::Statement&, Span, FileContext::ID, AssignedMap&, MovedMap&);

	void check_assigned(IR::BasicBlock&, IR::Function&, FileContext::ID, AssignedMap&, MovedMap&, PredsMap&);
	void check_assigned(IR::Function&, FileContext::ID);
	void check_assigned(IR::Module&, FileContext::ID);
	/// Checks whether each value has been assigned once before being used. Also checks mutability for set
	/// statements, allowing constant variables to be set once if they were originally set to be undefined, and for
	/// references. Enforces move semantics.
	void check_assigned();
};
