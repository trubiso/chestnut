#pragma once
#include "ast/function.hpp"
#include "ast/identifier.hpp"
#include "ast/module.hpp"
#include "ir.hpp"
#include "lexer.hpp"

#include <cstdint>
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
		, module_table_ {} {}

	/// Resolves all names and types and lowers to IR.
	std::vector<IR::Module> resolve();
	/// Exports all symbols as IR symbols.
	std::vector<IR::Symbol> export_symbols();
	/// Prints a dump of all symbols and their corresponding types.
	void dump() const;
	/// Prints an IR module using the symbol table.
	std::ostream& print(std::ostream&, IR::Module const&) const;

private:
	// === STRUCTS ===

	/// Holds information for a type during type inference.
	struct TypeInfo {
		typedef uint32_t ID;

		struct Function {
			// TODO: arguments can have default values! (size_t default_up_to;)
			std::vector<std::tuple<std::optional<std::string>, ID>> arguments;
			ID                                                      return_;
		};

		struct SameAs {
			std::vector<ID> ids;
		};

		struct Pointer {
			ID   pointee;
			bool mutable_;
		};

		struct KnownInteger {
			/// This may not be of width type Any!
			AST::Type::Atom::Integer integer;
		};

		struct KnownFloat {
			AST::Type::Atom::Float::Width width;
		};

		struct PartialInteger {
			/// This may be any kind of integer. If the signedness is not known (signed_is_known), it will
			/// be ignored.
			AST::Type::Atom::Integer integer;
			/// Whether the signedness for this integer is known.
			bool signed_is_known;
		};

		enum class Kind {
			/// Completely unknown type.
			Unknown,
			/// Unifies with anything, to avoid throwing more errors than needed.
			Bottom,
			/// Represents a module. For now, it does not hold any extra information.
			Module,
			/// Represents a function.
			Function,
			/// The exact same as any of the specified candidates.
			SameAs,
			/// A pointer type.
			Pointer,
			/// Known to be the built-in type 'void'.
			KnownVoid,
			/// Known to be the built-in type 'char'.
			KnownChar,
			/// Known to be the built-in type 'bool'.
			KnownBool,
			/// Known to be any of the specific 'int'/'uint' types.
			KnownInteger,
			/// Known to be any of the specific 'float' types.
			KnownFloat,
			/// Partially resolved integer type.
			PartialInteger,
			/// Partially resolved float type, that is, we know it is a float, but not which kind.
			PartialFloat,
		};

		typedef std::variant<
			std::monostate,  // Unknown
			std::monostate,  // Bottom
			std::monostate,  // Module
			Function,        // Function
			SameAs,          // SameAs
			Pointer,         // Pointer
			std::monostate,  // KnownVoid
			std::monostate,  // KnownChar
			std::monostate,  // KnownBool
			KnownInteger,    // KnownInteger
			KnownFloat,      // KnownFloat
			PartialInteger,  // PartialInteger
			std::monostate   // PartialFloat
			>
			value_t;

		value_t value;

		inline constexpr Kind kind() const { return (Kind) value.index(); }

		inline static TypeInfo make_unknown() {
			return TypeInfo(value_t {std::in_place_index<(size_t) Kind::Unknown>, std::monostate {}});
		}

		inline static TypeInfo make_bottom() {
			return TypeInfo(value_t {std::in_place_index<(size_t) Kind::Bottom>, std::monostate {}});
		}

		inline static TypeInfo make_module() {
			return TypeInfo(value_t {std::in_place_index<(size_t) Kind::Module>, std::monostate {}});
		}

		inline static TypeInfo make_function(Function&& function) {
			return TypeInfo(value_t {std::in_place_index<(size_t) Kind::Function>, std::move(function)});
		}

		inline static TypeInfo make_same_as(ID id) {
			return TypeInfo(value_t {std::in_place_index<(size_t) Kind::SameAs>, SameAs {{id}}});
		}

		inline static TypeInfo make_same_as(std::vector<ID>&& ids) {
			return TypeInfo(value_t {std::in_place_index<(size_t) Kind::SameAs>, SameAs {std::move(ids)}});
		}
		
		inline static TypeInfo make_pointer(Pointer&& pointer) {
			return TypeInfo(value_t {std::in_place_index<(size_t) Kind::Pointer>, std::move(pointer)});
		}

		inline static TypeInfo make_known_void() {
			return TypeInfo(value_t {std::in_place_index<(size_t) Kind::KnownVoid>, std::monostate {}});
		}

		inline static TypeInfo make_known_char() {
			return TypeInfo(value_t {std::in_place_index<(size_t) Kind::KnownChar>, std::monostate {}});
		}

		inline static TypeInfo make_known_bool() {
			return TypeInfo(value_t {std::in_place_index<(size_t) Kind::KnownBool>, std::monostate {}});
		}

		inline static TypeInfo make_known_integer(KnownInteger&& known_integer) {
			return TypeInfo(
				value_t {std::in_place_index<(size_t) Kind::KnownInteger>, std::move(known_integer)}
			);
		}

		inline static TypeInfo make_known_float(KnownFloat&& known_float) {
			return TypeInfo(
				value_t {std::in_place_index<(size_t) Kind::KnownFloat>, std::move(known_float)}
			);
		}

		inline static TypeInfo make_partial_integer(PartialInteger&& partial_integer) {
			return TypeInfo(
				value_t {std::in_place_index<(size_t) Kind::PartialInteger>, std::move(partial_integer)}
			);
		}

		inline static TypeInfo make_partial_float() {
			return TypeInfo(value_t {std::in_place_index<(size_t) Kind::PartialFloat>, std::monostate {}});
		}

		inline bool is_unknown() const { return kind() == Kind::Unknown; }

		inline bool is_bottom() const { return kind() == Kind::Bottom; }

		inline bool is_module() const { return kind() == Kind::Module; }

		inline bool is_function() const { return kind() == Kind::Function; }

		inline bool is_same_as() const { return kind() == Kind::SameAs; }

		inline bool is_pointer() const { return kind() == Kind::Pointer; }

		inline bool is_known_void() const { return kind() == Kind::KnownVoid; }

		inline bool is_known_char() const { return kind() == Kind::KnownChar; }

		inline bool is_known_bool() const { return kind() == Kind::KnownBool; }

		inline bool is_known_integer() const { return kind() == Kind::KnownInteger; }

		inline bool is_known_float() const { return kind() == Kind::KnownFloat; }

		inline bool is_partial_integer() const { return kind() == Kind::PartialInteger; }

		inline bool is_partial_float() const { return kind() == Kind::PartialFloat; }

		inline Function const& get_function() const { return std::get<(size_t) Kind::Function>(value); }

		inline Function& get_function() { return std::get<(size_t) Kind::Function>(value); }

		inline SameAs const& get_same_as() const { return std::get<(size_t) Kind::SameAs>(value); }
		
		inline Pointer const& get_pointer() const { return std::get<(size_t) Kind::Pointer>(value); }
		
		inline Pointer& get_pointer() { return std::get<(size_t) Kind::Pointer>(value); }

		inline KnownInteger const& get_known_integer() const {
			return std::get<(size_t) Kind::KnownInteger>(value);
		}

		inline KnownFloat const& get_known_float() const { return std::get<(size_t) Kind::KnownFloat>(value); }

		inline PartialInteger const& get_partial_integer() const {
			return std::get<(size_t) Kind::PartialInteger>(value);
		}

		inline PartialInteger& get_partial_integer() { return std::get<(size_t) Kind::PartialInteger>(value); }

		static TypeInfo from_type(AST::Type::Atom const&);

		/// Returns whether this type pertains to something which could theoretically be callable.
		bool is_callable(std::vector<TypeInfo> const&) const;
		/// Returns whether this type pertains to something which could theoretically be dereferenceable.
		bool is_pointer(std::vector<TypeInfo> const&) const;
		/// Returns all callable subitems within this type.
		std::vector<ID> get_callable_subitems(ID self_id, std::vector<TypeInfo> const&) const;
		/// Returns the pointee for this pointer type (assumes is_pointer()).
		ID get_pointee(std::vector<TypeInfo> const&) const;
		/// Returns whether this pointer is mutable (assumes is_pointer()).
		bool get_pointer_mutable(std::vector<TypeInfo> const&) const;
	};

	struct Symbol {
		AST::SymbolID   id;
		FileContext::ID file_id;
		Span            span;
		std::string     name;

		/// Holds the module item that this points to, if any.
		std::variant<
			AST::Module*,
			AST::Function*,
			AST::Struct*,
			IR::Module,
			IR::Function,
			IR::BuiltInFunction,
			std::monostate>
			item;

		TypeInfo::ID type;

		/// Whether this symbol can be mutated (should be true only for mutable declarations and arguments).
		bool mutable_;
		/// Whether this symbol can be imported (should be set to false in non-module items)
		bool exported;

		/// Which files this symbol is imported from.
		std::vector<FileContext::ID> imported_from;

		bool is_visible(FileContext::ID) const;
	};

	// TODO: store scopes so we can import symbols from scopes

	/// Holds a scope's symbols for unqualified identifier resolution.
	struct Scope {
		/// We store the parent to traverse up scopes until we find the symbol.
		Scope const* parent = nullptr;
		/// A name may point to more than one ID (e.g. for overloaded functions).
		std::unordered_map<std::string, std::vector<AST::SymbolID>> symbols;
	};

	/// Returns the file context for the requested file ID.
	FileContext get_context(FileContext::ID) const;

	// === POPULATION ===

	/// Holds all the top-level modules (that is, files) in an unordered map.
	std::unordered_map<std::string, AST::Module*> module_table_;

	/// Populates the module table according to the parsed files.
	void populate_module_table();

	// === IDENTIFICATION ===

	/// Produces a single ID for the identifier and sets it; the caller must register this ID in the symbol pool.
	void identify(AST::Identifier&);
	/// Identifies the module with an ID and its non-alias items.
	void identify(AST::Module&, bool exported, FileContext::ID);
	/// Identifies the struct with an ID.
	void identify(AST::Struct&, bool exported, FileContext::ID);
	/// Identifies the function with an ID.
	void identify(AST::Function&, bool exported, FileContext::ID);
	/// Identifies all module items with an ID, but does not resolve aliases.
	void identify_module_items();
	/// Identifies a built-in operator.
	void identify_built_in_operator(IR::BuiltInFunction, Token::Symbol, TypeInfo&&);
	/// Identifies a built-in unary operator which returns the same type as it takes in.
	void identify_built_in_unary_operator(IR::BuiltInFunction, Token::Symbol, TypeInfo&&);
	/// Identifies a built-in deref operator which takes a pointer to the provided type and returns the provided type.
	void identify_built_in_unary_deref_operator(IR::BuiltInFunction, TypeInfo&&);
	/// Identifies a built-in address operator which takes the provided type and returns a pointer to the provided type.
	void identify_built_in_unary_address_operator(IR::BuiltInFunction, Token::Symbol, TypeInfo&&);
	/// Identifies a built-in binary operator which returns the same type as it takes in.
	void identify_built_in_binary_operator(IR::BuiltInFunction, Token::Symbol, TypeInfo&&);
	/// Identifies a built-in binary comparison operator (returning a boolean value).
	void identify_built_in_binary_comparison_operator(IR::BuiltInFunction, Token::Symbol, TypeInfo&&);
	/// Identifies all built-in operators.
	void identify_built_in_operators();
	/// Populates the label map given a statement.
	void identify_populate_labels(
		Spanned<AST::Statement>&,
		std::unordered_map<std::string, Spanned<AST::Statement::Label::ID>>&,
		AST::Statement::Label::ID& counter,
		FileContext::ID
	);
	/// Adds an "unknown label" diagnostic at the provided span and file ID.
	void identify_add_unknown_label_diagnostic(Span, FileContext::ID);
	/// Identifies all labels and statements using them given a statement and a mapping from names to label IDs.
	void identify_labels(
		Spanned<AST::Statement>&,
		std::unordered_map<std::string, Spanned<AST::Statement::Label::ID>> const&,
		FileContext::ID
	);
	/// Identifies all labels and statements using them in a given function.
	void identify_labels(AST::Function&, FileContext::ID);
	/// Identifies all labels and statements using them in all functions in a given module.
	void identify_labels(AST::Module&, FileContext::ID);
	/// Identifies all labels and statements using them in all functions.
	void identify_labels();

	// === DESUGAR ===

	std::vector<Spanned<AST::Statement>> desugar_control_flow_expr_binop(AST::Expression&, Span, AST::Statement::Label::ID& label_counter, FileContext::ID);
	std::vector<Spanned<AST::Statement>> desugar_control_flow_expr_if(AST::Expression&, Span, AST::Statement::Label::ID& label_counter, FileContext::ID);

	std::vector<Spanned<AST::Statement>> desugar_control_flow_expr(AST::Expression::UnaryOperation&, AST::Statement::Label::ID& label_counter, FileContext::ID);
	std::vector<Spanned<AST::Statement>> desugar_control_flow_expr(AST::Expression::AddressOperation&, AST::Statement::Label::ID& label_counter, FileContext::ID);
	std::vector<Spanned<AST::Statement>> desugar_control_flow_expr(AST::Expression::BinaryOperation&, AST::Statement::Label::ID& label_counter, FileContext::ID);
	std::vector<Spanned<AST::Statement>> desugar_control_flow_expr(AST::Expression::FunctionCall&, Span, AST::Statement::Label::ID& label_counter, FileContext::ID);
	std::vector<Spanned<AST::Statement>> desugar_control_flow_expr(AST::Expression&, Span, AST::Statement::Label::ID& label_counter, FileContext::ID);
	/// Desugars all higher-level control-flow structures within an expression, returning any necessary statements to be inserted before it.
	std::vector<Spanned<AST::Statement>> desugar_control_flow_expr(Spanned<AST::Expression>&, AST::Statement::Label::ID& label_counter, FileContext::ID);

	std::vector<Spanned<AST::Statement>> desugar_control_flow(AST::Statement::Declare&&, Span, AST::Statement::Label::ID& label_counter, FileContext::ID);
	std::vector<Spanned<AST::Statement>> desugar_control_flow(AST::Statement::Set&&, Span, AST::Statement::Label::ID& label_counter, FileContext::ID);
	std::vector<Spanned<AST::Statement>> desugar_control_flow(AST::Expression&&, Span, AST::Statement::Label::ID& label_counter, FileContext::ID);
	std::vector<Spanned<AST::Statement>> desugar_control_flow(AST::Statement::Return&&, Span, AST::Statement::Label::ID& label_counter, FileContext::ID);
	std::vector<Spanned<AST::Statement>> desugar_control_flow(AST::Statement::Branch&&, Span, AST::Statement::Label::ID& label_counter, FileContext::ID);
	std::vector<Spanned<AST::Statement>> desugar_control_flow(AST::Statement::If&&, Span, AST::Statement::Label::ID& label_counter, FileContext::ID);
	/// Desugars all higher-level control-flow structures within a statement, returning its transformation into a set of statements.
	std::vector<Spanned<AST::Statement>> desugar_control_flow(AST::Statement&&, Span, AST::Statement::Label::ID& label_counter, FileContext::ID);

	/// Desugars all higher-level control-flow structures within a scope.
	AST::Scope desugar_control_flow(AST::Scope&&, AST::Statement::Label::ID& label_counter, FileContext::ID);
	/// Desugars all higher-level control-flow structures within a function.
	void desugar_control_flow(AST::Function&, FileContext::ID);
	/// Desugars all higher-level control-flow structures within a module.
	void desugar_control_flow(AST::Module&, FileContext::ID);
	/// Desugars all higher-level control-flow structures into combinations of labels, goto and branch.
	void desugar_control_flow();

	// === SYMBOLS ===

	/// Holds all symbols. All valid AST::SymbolIDs are valid indices to this array.
	std::vector<Symbol> symbol_pool_;
	/// Holds the symbol ID one above the last valid symbol ID.
	AST::SymbolID symbol_counter_ = 0;

	/// Returns a new ID produced by the counter.
	AST::SymbolID symbol_next();

	/// Gets a symbol from the symbol pool.
	inline Symbol& get_single_symbol(AST::SymbolID id) { return symbol_pool_.at(id); }

	/// Gets a symbol from the symbol pool, assuming the identifier is fully resolved.
	inline Symbol& get_single_symbol(AST::Identifier const& identifier) {
		assert(identifier.id.has_value() && identifier.id.value().size() == 1);
		return get_single_symbol(identifier.id.value().at(0));
	}

	/// Adds a diagnostic indicating a symbol is unknown.
	void add_unknown_symbol_diagnostic(
		std::string_view symbol,
		Span             span,
		/// A list of the names of all candidates (the closest are determined by Levenshtein distance).
		std::vector<std::string> const& possible_symbols,
		/// Which kind of scope the symbol was not found in (should be just an adjective).
		std::string_view scope_type,
		FileContext::ID,
		/// Whether a suggestion to import a symbol with the same name should be added to the diagnostic.
		bool add_import_suggestion = false
	);

	void resolve(AST::Identifier&, Span, Scope const&, FileContext::ID, bool include_unimported = false);
	void resolve(Spanned<AST::Identifier>&, Scope const&, FileContext::ID, bool include_unimported = false);
	void resolve(AST::Expression::UnaryOperation&, Scope const&, FileContext::ID);
	void resolve(AST::Expression::AddressOperation&, Scope const&, FileContext::ID);
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

	// === TYPES ===

	/// Holds all types. All valid TypeInfo::IDs are valid indices to this array.
	std::vector<TypeInfo> type_pool_;
	/// Holds the span and file ID for all types. All valid TypeInfo::IDs are valid indices to this array.
	std::vector<std::tuple<Span, FileContext::ID>> type_span_pool_;
	/// Holds the corresponding symbol for some types. All valid TypeInfo::IDs are valid indices to this array.
	std::vector<std::optional<AST::SymbolID>> type_symbol_mapping_;
	/// Holds the type ID one above the last valid type ID.
	TypeInfo::ID type_counter_ = 0;

	struct UndecidedOverload {
		TypeInfo::ID                    expr_type;
		std::optional<AST::Identifier*> identifier;

		struct Candidate {
			TypeInfo::ID function;
			TypeInfo call_type;
		};

		std::vector<Candidate>          candidates;
		std::vector<Diagnostic::Sample> rejections;
		Span                            span;
		FileContext::ID                 file_id;
	};

	std::vector<UndecidedOverload> undecided_overloads {};

	TypeInfo from_type(AST::Type::Pointer const&, FileContext::ID);
	TypeInfo from_type(AST::Type const&, FileContext::ID);

	/// Returns the span for a given type ID.
	inline Span get_type_span(TypeInfo::ID id) const { return std::get<0>(type_span_pool_.at(id)); }

	/// Returns the file ID for a given type ID.
	inline FileContext::ID get_type_file_id(TypeInfo::ID id) const { return std::get<1>(type_span_pool_.at(id)); }

	/// Prints debug info for a type given its ID, without a newline.
	void debug_print_type(TypeInfo::ID) const;
	/// Prints debug info for a type, without a newline.
	void debug_print_type(TypeInfo) const;

	/// Returns a name for a type suitable for a diagnostic.
	std::string get_type_name(TypeInfo::ID) const;
	/// Returns a name for a type suitable for a diagnostic.
	std::string get_type_name(TypeInfo const&) const;
	/// Returns a type sample for the provided type ID.
	Diagnostic::Sample get_type_sample(TypeInfo::ID, OutFmt::Color) const;

	/// Returns a new ID produced by the type counter.
	TypeInfo::ID type_next();

	/// Registers a type in the type pool and returns its ID.
	TypeInfo::ID register_type(TypeInfo&&, Span, FileContext::ID, std::optional<AST::SymbolID> = {});

	/// Gets all candidate functions for an operator.
	std::vector<AST::SymbolID> get_operator_candidates(Token::Symbol operator_, bool binary) const;

	/// Sets two types to be the same, avoiding any SameAs cycles. Never fails!
	void set_same_as(TypeInfo::ID to, TypeInfo::ID from);
	/// Follows references and returns TypeInfo::SameAs with the new roster or TypeInfo::Bottom if none can be
	/// unified (throws a diagnostic in that case).
	TypeInfo unify_follow_references(
		TypeInfo::ID same_as,
		TypeInfo::ID other,
		TypeInfo::ID same_as_origin,
		TypeInfo::ID other_origin,
		FileContext::ID
	);
	/// Handles the case of basic known types which have no extra information, returns whether any of the provided
	/// types matched the type kind.
	bool unify_basic_known(
		TypeInfo::Kind,
		TypeInfo::ID a,
		TypeInfo::ID b,
		TypeInfo::ID a_origin,
		TypeInfo::ID b_origin,
		FileContext::ID
	);
	/// Unifies a function and another type.
	void unify_functions(
		TypeInfo::ID function,
		TypeInfo::ID other,
		TypeInfo::ID function_origin,
		TypeInfo::ID other_origin,
		FileContext::ID
	);
	/// Unifies a pointer and another type.
	void unify_pointers(
		TypeInfo::ID pointer,
		TypeInfo::ID other,
		TypeInfo::ID pointer_origin,
		TypeInfo::ID other_origin,
		FileContext::ID
	);
	/// Equates two types and adds a diagnostic if it fails, specifying the original type IDs for diagnostics.
	void unify(TypeInfo::ID a, TypeInfo::ID b, TypeInfo::ID a_origin, TypeInfo::ID b_origin, FileContext::ID);
	/// Equates two types and adds a diagnostic if it fails.
	void unify(TypeInfo::ID, TypeInfo::ID, FileContext::ID);

	/// Returns whether any of the references in a TypeInfo::SameAs can be followed to be unified.
	bool can_unify_follow_references(TypeInfo const& same_as, TypeInfo const&) const;
	/// Returns whether a basic known type can be unified with another one, given it matches, otherwise returns
	/// null.
	std::optional<bool> can_unify_basic_known(TypeInfo::Kind, TypeInfo const&, TypeInfo const&) const;
	/// Returns whether a function and another type can be unified.
	bool can_unify_functions(TypeInfo const& function, TypeInfo const&) const;
	/// Returns whether a pointer and another type can be unified.
	bool can_unify_pointers(TypeInfo const& pointer, TypeInfo const&) const;
	/// Returns whether two types can be unified.
	bool can_unify(TypeInfo::ID, TypeInfo::ID) const;
	bool can_unify(TypeInfo const&, TypeInfo::ID) const;
	bool can_unify(TypeInfo::ID, TypeInfo const&) const;
	bool can_unify(TypeInfo const&, TypeInfo const&) const;

	/// Tries to decide an undecided overload. Returns whether the overload was successfully decided (that
	/// includes the case in which it is determined that no function meets the constraints!)
	bool try_decide(UndecidedOverload&);

	TypeInfo::ID infer(AST::Expression::Atom const&, Span, FileContext::ID);
	TypeInfo::ID infer(AST::Expression::FunctionCall&, Span, FileContext::ID);
	TypeInfo::ID infer(AST::Expression&, Span, FileContext::ID);
	void         infer(AST::Statement::Declare&, FileContext::ID);
	void         infer(AST::Statement::Set&, FileContext::ID);
	void         infer(AST::Statement::Return&, Span, AST::SymbolID function, FileContext::ID);
	void         infer(Spanned<AST::Statement>&, AST::SymbolID function, FileContext::ID);
	void         infer(AST::Scope&, AST::SymbolID function, FileContext::ID);
	void         infer(AST::Function&, FileContext::ID);
	void         infer(AST::Module&, FileContext::ID);
	/// Infers all types within the program.
	void infer_types();

	// === LOWERING ===

	/// Reconstructs an inferred type, throwing a diagnostic if it is still unknown. This should be used only for
	/// values and expressions, as it does not allow functions or modules to be values directly. It also takes in a
	/// type origin, which overrides the type ID for diagnostics.
	IR::Type reconstruct_type(TypeInfo::ID type_id, TypeInfo::ID type_origin, bool allow_functions = false);
	/// Reconstructs an inferred type, throwing a diagnostic if it is still unknown. This should be used only for
	/// values and expressions, as it does not allow functions or modules to be values directly.
	IR::Type reconstruct_type(TypeInfo::ID, bool allow_functions = false);
	/// Lowers a type atom known since parsing, turning non-specific types specific.
	Spanned<IR::Type> lower_type(AST::Type::Atom, Span, FileContext::ID);
	/// Lowers a type known since parsing, turning non-specific types specific.
	Spanned<IR::Type> lower_type(Spanned<AST::Type>, FileContext::ID);

	/// Lowers any identifier into its IR equivalent and type.
	std::optional<std::tuple<Spanned<IR::Identifier>, IR::Type>> lower(Spanned<AST::Identifier> const&, bool allow_functions = false);
	/// Lowers any identifier into its IR equivalent and type.
	std::optional<std::tuple<IR::Identifier, IR::Type>> lower(AST::Identifier const&, bool allow_functions = false);
	/// Lowers an identifier which should ALWAYS be resolved (i.e. those of non-alias module items).
	Spanned<IR::Identifier> lower_identifier(Spanned<AST::Identifier> const&);

	Spanned<IR::Expression::Atom> extract_expression(AST::Expression const&, Span, std::vector<IR::BasicBlock>&, FileContext::ID, bool allow_functions = false);
	Spanned<IR::Expression::Atom> extract_expression(Spanned<AST::Expression> const&, std::vector<IR::BasicBlock>&, FileContext::ID, bool allow_functions = false);
	IR::Expression::Atom lower_atom(AST::Expression::Atom const&, TypeInfo::ID, Span, std::vector<IR::BasicBlock>&, FileContext::ID, bool allow_functions = false);
	// FIXME: clang-format goes BALLISTIC over these function declarations
	Spanned<IR::Expression> lower(AST::Expression::FunctionCall const&, Span, std::vector<IR::BasicBlock>&, FileContext::ID, bool allow_functions = false);
	Spanned<IR::Expression> lower(AST::Expression::Atom const&, TypeInfo::ID, Span, std::vector<IR::BasicBlock>&, FileContext::ID, bool allow_functions = false);
	Spanned<IR::Expression> lower(AST::Expression const&, Span, std::vector<IR::BasicBlock>&, FileContext::ID, bool allow_functions = false);
	Spanned<IR::Expression> lower(Spanned<AST::Expression> const&, std::vector<IR::BasicBlock>&, FileContext::ID, bool allow_functions = false);
	std::optional<Spanned<IR::Statement>> lower(AST::Statement::Declare const&, Span, std::vector<IR::BasicBlock>&, FileContext::ID);
	std::optional<Spanned<IR::Statement>> lower(AST::Statement::Set const&, Span, std::vector<IR::BasicBlock>&, FileContext::ID);
	std::optional<Spanned<IR::Statement>> lower(Spanned<AST::Statement> const&, AST::Function&, std::vector<IR::BasicBlock>&, FileContext::ID);
	void                                  lower(std::vector<Spanned<AST::Statement>> const&, AST::Function&, std::vector<IR::BasicBlock>&, FileContext::ID);
	IR::Function                          lower(AST::Function&, FileContext::ID);
	IR::Module                            lower(AST::Module&, FileContext::ID);
	/// Lowers all files into IR modules.
	std::vector<IR::Module> lower();
};
