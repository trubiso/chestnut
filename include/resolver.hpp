#pragma once
#include "ast/function.hpp"
#include "ast/identifier.hpp"
#include "ast/module.hpp"
#include "ir.hpp"
#include "lexer.hpp"

#include <cstdint>
#include <unordered_map>
#include <unordered_set>

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
	std::vector<IR::Module> resolve(bool print_ir = false);
	/// Exports all symbols as IR symbols.
	std::vector<IR::Symbol> export_symbols();
	/// Prints a dump of all symbols and their corresponding types.
	void dump(bool print_built_ins = false) const;
	/// Prints an IR module using the symbol table.
	std::ostream& print(std::ostream&, IR::Module const&) const;

private:
	// === STRUCTS ===

	struct SubstList {
		inline uint32_t push() {
			uint32_t id = nodes.size();

			Node node {.parent = id, .size = 1};
			nodes.push_back(std::move(node));
			return id;
		}

		inline uint32_t get(uint32_t id) {
			return (nodes.at(id).parent != id) ? (nodes.at(id).parent = get(nodes.at(id).parent)) : id;
		}

		inline void merge(uint32_t x, uint32_t y) {
			x = get(x);
			y = get(y);
			if (x == y) return;

			if (nodes.at(x).size < nodes.at(y).size) std::swap(x, y);

			nodes.at(y).parent = x;

			nodes.at(x).size += nodes.at(y).size;
		}
	private:
		struct Node { uint32_t parent; uint32_t size; };
		std::vector<Node> nodes;
	};

	// TODO: hold generics in named types

	/// Holds information for a name during inference.
	struct NameVar {
		typedef uint32_t ID;

		inline bool has_value() const { return value_.has_value(); }

		inline AST::SymbolID value() const {
			assert(has_value());
			return value_.value();
		}

		inline void set(AST::SymbolID value) {
			assert(!has_value());
			value_ = value;
		}

		inline NameVar() : value_ {std::nullopt} {}

		explicit inline NameVar(AST::SymbolID value) : value_ {value} {}

	private:
		/// The concrete symbol that this name points to.
		std::optional<AST::SymbolID> value_;
	};

	/// Holds information for a type during inference.
	struct TypeVar {
		typedef uint32_t ID;

		struct Named {
			NameVar::ID name;
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
			/// A named type with potential generics.
			Named,
			/// A pointer type.
			Pointer,
			/// Known to be the built-in type `void`.
			Void,
			/// Known to be the built-in type `char`.
			Char,
			/// Known to be the built-in type `bool`.
			Bool,
			/// Known to be any of the specific `int`/`uint` types.
			Integer,
			/// Known to be any of the specific `float` types.
			Float,
			/// Partially resolved integer type.
			PartialInteger,
			/// Partially resolved float type, that is, we know it is a float, but not which kind.
			PartialFloat,
		};

		typedef std::variant<
			std::monostate,  // Unknown
			std::monostate,  // Bottom
			std::monostate,  // Module
			Named,           // Named
			Pointer,         // Pointer
			std::monostate,  // Void
			std::monostate,  // Char
			std::monostate,  // Bool
			KnownInteger,    // Integer
			KnownFloat,      // Float
			PartialInteger,  // PartialInteger
			std::monostate   // PartialFloat
			>
			value_t;

		value_t value;

		inline constexpr Kind kind() const { return (Kind) value.index(); }

		inline static TypeVar make_unknown() {
			return TypeVar(value_t {std::in_place_index<(size_t) Kind::Unknown>, std::monostate {}});
		}

		inline static TypeVar make_bottom() {
			return TypeVar(value_t {std::in_place_index<(size_t) Kind::Bottom>, std::monostate {}});
		}

		inline static TypeVar make_module() {
			return TypeVar(value_t {std::in_place_index<(size_t) Kind::Module>, std::monostate {}});
		}

		inline static TypeVar make_named(NameVar::ID name) {
			return TypeVar(value_t {std::in_place_index<(size_t) Kind::Named>, Named {name}});
		}

		inline static TypeVar make_pointer(ID pointee, bool mutable_) {
			return TypeVar(value_t {std::in_place_index<(size_t) Kind::Pointer>, Pointer {pointee, mutable_}});
		}

		inline static TypeVar make_void() {
			return TypeVar(value_t {std::in_place_index<(size_t) Kind::Void>, std::monostate {}});
		}

		inline static TypeVar make_char() {
			return TypeVar(value_t {std::in_place_index<(size_t) Kind::Char>, std::monostate {}});
		}

		inline static TypeVar make_bool() {
			return TypeVar(value_t {std::in_place_index<(size_t) Kind::Bool>, std::monostate {}});
		}

		inline static TypeVar make_integer(KnownInteger&& known_integer) {
			return TypeVar(value_t {std::in_place_index<(size_t) Kind::Integer>, std::move(known_integer)});
		}

		inline static TypeVar make_float(KnownFloat&& known_float) {
			return TypeVar(value_t {std::in_place_index<(size_t) Kind::Float>, std::move(known_float)});
		}

		inline static TypeVar make_partial_integer(PartialInteger&& partial_integer) {
			return TypeVar(
				value_t {std::in_place_index<(size_t) Kind::PartialInteger>, std::move(partial_integer)}
			);
		}

		inline static TypeVar make_partial_float() {
			return TypeVar(value_t {std::in_place_index<(size_t) Kind::PartialFloat>, std::monostate {}});
		}

		inline bool is_unknown() const { return kind() == Kind::Unknown; }

		inline bool is_bottom() const { return kind() == Kind::Bottom; }

		inline bool is_module() const { return kind() == Kind::Module; }

		inline bool is_named() const { return kind() == Kind::Named; }

		inline bool is_pointer() const { return kind() == Kind::Pointer; }

		inline bool is_void() const { return kind() == Kind::Void; }

		inline bool is_char() const { return kind() == Kind::Char; }

		inline bool is_bool() const { return kind() == Kind::Bool; }

		inline bool is_integer() const { return kind() == Kind::Integer; }

		inline bool is_float() const { return kind() == Kind::Float; }

		inline bool is_partial_integer() const { return kind() == Kind::PartialInteger; }

		inline bool is_partial_float() const { return kind() == Kind::PartialFloat; }

		inline Named const& get_named() const { return std::get<(size_t) Kind::Named>(value); }

		inline Named& get_named() { return std::get<(size_t) Kind::Named>(value); }

		inline Pointer const& get_pointer() const { return std::get<(size_t) Kind::Pointer>(value); }

		inline Pointer& get_pointer() { return std::get<(size_t) Kind::Pointer>(value); }

		inline KnownInteger const& get_integer() const { return std::get<(size_t) Kind::Integer>(value); }

		inline KnownFloat const& get_float() const { return std::get<(size_t) Kind::Float>(value); }

		inline PartialInteger const& get_partial_integer() const {
			return std::get<(size_t) Kind::PartialInteger>(value);
		}

		inline PartialInteger& get_partial_integer() { return std::get<(size_t) Kind::PartialInteger>(value); }
	};

	struct Constraint {
		struct NameEquality {
			NameVar::ID a;
			NameVar::ID b;
		};

		struct TypeEquality {
			TypeVar::ID a;
			TypeVar::ID b;
		};

		struct Subtype {
			TypeVar::ID a;
			TypeVar::ID b;
		};

		struct Never {};

		std::variant<NameEquality, TypeEquality, Subtype, Never> value;
	};

	struct Branch;

	struct Statement {
		std::vector<std::variant<Constraint, Branch>> constraints;
	};

	struct Branch {
		std::vector<Statement> statements;
	};

	struct Generic {};

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
			AST::Trait*,
			Generic,  // this is just a marker!
			IR::Module,
			IR::Function,
			IR::BuiltInFunction,
			IR::Struct,
			std::monostate>
			item;

		TypeVar::ID type;

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
	
	std::unordered_map<std::string, AST::Trait> built_in_traits_ {};

	/// Registers a symbol for the provided name and sets its ID to the registered symbol's ID.
	void identify(Spanned<AST::Name>&, decltype(Symbol::item), bool exported, TypeVar, FileContext::ID);
	/// Identifies the module with an ID and its non-alias items.
	void identify(AST::Module&, bool exported, FileContext::ID);
	/// Identifies the struct with an ID.
	void identify(AST::Struct&, bool exported, FileContext::ID);
	/// Identifies the trait with an ID and its methods.
	void identify(AST::Trait&, bool exported, FileContext::ID);
	/// Identifies the trait implementation's methods.
	void identify(AST::TraitImplementation&, bool exported, FileContext::ID);
	/// Identifies the function with an ID.
	void identify(AST::Function&, bool exported, FileContext::ID);
	/// Identifies a generic declaration's generics with stub types later filled in by the symbols phase.
	void identify(AST::GenericDeclaration&, FileContext::ID);
	/// Identifies all module items with an ID, but does not resolve aliases.
	void identify_module_items();
	/// Creates a generic type for a built-in operator.
	TypeVar::ID create_built_in_generic(std::string&& name, std::string&& trait_bound);
	/// Identifies a built-in operator.
	void identify_built_in_operator(IR::BuiltInFunction, Token::Symbol, TypeVar::ID);
	/// Identifies a built-in unary operator which returns the same type as it takes in.
	void identify_built_in_unary_operator(IR::BuiltInFunction, Token::Symbol, TypeVar::ID, bool generic = false);
	/// Identifies a built-in binary operator which returns the same type as it takes in.
	void identify_built_in_binary_operator(IR::BuiltInFunction, Token::Symbol, TypeVar::ID, bool generic = false);
	/// Identifies a built-in binary comparison operator (returning a boolean value).
	void identify_built_in_binary_comparison_operator(IR::BuiltInFunction, Token::Symbol, TypeVar::ID, bool generic = false);
	/// Identifies all built-in operators.
	void identify_built_in_operators();
	/// Pushes a particular built-in trait to the symbol pool.
	void push_built_in_trait(AST::Name const&, std::vector<AST::SymbolID>&& constraints = {});
	/// Makes a built-in name.
	Spanned<AST::Name> make_built_in_name(std::string name);
	/// Identifies all built-in traits.
	void identify_built_in_traits();
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

	struct LoopCtx {
		AST::Statement::Label::ID where_to_continue;
		AST::Statement::Label::ID where_to_break;
	};

	std::vector<Spanned<AST::Statement>> desugar_control_flow_expr_binop(AST::Expression&, Span, AST::Statement::Label::ID& label_counter, FileContext::ID);
	std::vector<Spanned<AST::Statement>> desugar_control_flow_expr_if(AST::Expression&, Span, AST::Statement::Label::ID& label_counter, FileContext::ID);

	std::vector<Spanned<AST::Statement>> desugar_control_flow_expr(AST::Expression::Atom&, Span, AST::Statement::Label::ID& label_counter, FileContext::ID);
	std::vector<Spanned<AST::Statement>> desugar_control_flow_expr(AST::Expression::UnaryOperation&, AST::Statement::Label::ID& label_counter, FileContext::ID);
	std::vector<Spanned<AST::Statement>> desugar_control_flow_expr(AST::Expression::AddressOperation&, AST::Statement::Label::ID& label_counter, FileContext::ID);
	std::vector<Spanned<AST::Statement>> desugar_control_flow_expr(AST::Expression::BinaryOperation&, AST::Statement::Label::ID& label_counter, FileContext::ID);
	std::vector<Spanned<AST::Statement>> desugar_control_flow_expr(AST::Expression::FunctionCall&, Span, AST::Statement::Label::ID& label_counter, FileContext::ID);
	std::vector<Spanned<AST::Statement>> desugar_control_flow_expr(AST::Expression::MemberAccess&, Span, AST::Statement::Label::ID& label_counter, FileContext::ID);
	std::vector<Spanned<AST::Statement>> desugar_control_flow_expr(AST::Expression&, Span, AST::Statement::Label::ID& label_counter, FileContext::ID);
	/// Desugars all higher-level control-flow structures within an expression, returning any necessary statements to be inserted before it.
	std::vector<Spanned<AST::Statement>> desugar_control_flow_expr(Spanned<AST::Expression>&, AST::Statement::Label::ID& label_counter, FileContext::ID);

	std::vector<Spanned<AST::Statement>> desugar_control_flow(AST::Statement::Declare&&, Span, AST::Statement::Label::ID& label_counter, FileContext::ID);
	std::vector<Spanned<AST::Statement>> desugar_control_flow(AST::Statement::Set&&, Span, AST::Statement::Label::ID& label_counter, FileContext::ID);
	std::vector<Spanned<AST::Statement>> desugar_control_flow(AST::Expression&&, Span, AST::Statement::Label::ID& label_counter, FileContext::ID);
	std::vector<Spanned<AST::Statement>> desugar_control_flow(AST::Statement::Return&&, Span, AST::Statement::Label::ID& label_counter, FileContext::ID);
	std::vector<Spanned<AST::Statement>> desugar_control_flow(AST::Statement::Branch&&, Span, AST::Statement::Label::ID& label_counter, FileContext::ID);
	std::vector<Spanned<AST::Statement>> desugar_control_flow(AST::Statement::If&&, Span, AST::Statement::Label::ID& label_counter, FileContext::ID, std::optional<LoopCtx> const&);
	std::vector<Spanned<AST::Statement>> desugar_control_flow(AST::Statement::While&&, Span, AST::Statement::Label::ID& label_counter, FileContext::ID);
	/// Desugars all higher-level control-flow structures within a statement, returning its transformation into a set of statements.
	std::vector<Spanned<AST::Statement>> desugar_control_flow(AST::Statement&&, Span, AST::Statement::Label::ID& label_counter, FileContext::ID, std::optional<LoopCtx> const&);

	/// Desugars all higher-level control-flow structures within a scope.
	AST::Scope desugar_control_flow(AST::Scope&&, AST::Statement::Label::ID& label_counter, FileContext::ID, std::optional<LoopCtx> const&);
	/// Desugars all higher-level control-flow structures within a trait implementation's methods.
	void desugar_control_flow(AST::TraitImplementation&, FileContext::ID);
	/// Desugars all higher-level control-flow structures within a trait's methods.
	void desugar_control_flow(AST::Trait&, FileContext::ID);
	/// Desugars all higher-level control-flow structures within a function.
	void desugar_control_flow(AST::Function&, FileContext::ID);
	/// Desugars all higher-level control-flow structures within a module.
	void desugar_control_flow(AST::Module&, FileContext::ID);
	/// Desugars all higher-level control-flow structures into combinations of labels, goto and branch.
	void desugar_control_flow();

	// === INFERENCE ===

	/// Holds all symbols. All valid AST::SymbolIDs are valid indices to this array.
	std::vector<Symbol> symbol_pool_;

	/// Substitution list for types.
	SubstList type_substs_ {};
	/// Substitution list for names.
	SubstList name_substs_ {};

	/// Mapping from NameVar::ID to NameVar. Ensure to apply substitutions before accessing.
	std::vector<NameVar> name_pool_ {};
	/// Mapping from TypeVar::ID to TypeVar. Ensure to apply substitutions before accessing.
	std::vector<TypeVar> type_pool_ {};

	/// Mapping from TypeVar::ID to its corresponding span and file ID.
	std::vector<std::tuple<Span, FileContext::ID>> type_span_pool_;

	/// Returns an ID to create a symbol (based on the current amount of symbols on the pool!)
	inline AST::SymbolID symbol_next() { return symbol_pool_.size(); };
	/// Returns an ID to create a name (based on the name substitution list)
	inline NameVar::ID name_next() {
		NameVar::ID id = name_substs_.push();
		assert(name_pool_.size() == id && "a name variable was created but not added to the pool");
		return id;	
	}
	/// Returns an ID to create a type (based on the type substitution list)
	inline TypeVar::ID type_next() {
		TypeVar::ID id = type_substs_.push();
		assert(type_pool_.size()      == id && "a type variable was created but not added to the pool");
		assert(type_span_pool_.size() == id && "a type variable was created but not added to the span pool");
		return id;
	}

	/// Registers a symbol and returns its ID (the ID, span and file ID fields are overwritten).
	AST::SymbolID register_symbol(Symbol, Span, FileContext::ID);
	/// Registers a name and returns its ID.
	NameVar::ID register_name(NameVar, Span, FileContext::ID);
	/// Registers a type and returns its ID.
	TypeVar::ID register_type(TypeVar, Span, FileContext::ID);

	/// Creates an unexported name with the provided type, mutability, span and file ID.
	AST::Name create_name(std::string_view name, TypeVar, bool mutable_, Span, FileContext::ID);

	/// Gets a symbol from the symbol pool.
	inline Symbol& get_single_symbol(AST::SymbolID id) { return symbol_pool_.at(id); }

	/// Gets a symbol from the symbol pool, assuming the identifier is fully decided.
	inline Symbol& get_single_symbol(AST::Identifier const& identifier) {
		assert(identifier.is_decided() && "attempted to obtain corresponding symbol for non-decided identifier");
		return get_single_symbol(identifier.id());
	}

	/// Gets a symbol from the symbol pool, assuming the name has been identified.
	inline Symbol& get_single_symbol(AST::Name const& name) {
		assert(name.id.has_value() && "attempted to obtain corresponding symbol for unidentified name");
		return get_single_symbol(name.id.value());
	}

	/// Gets the corresponding name variable for the provided name variable ID.
	inline NameVar& get_name_var(NameVar::ID id) { return name_pool_.at(name_substs_.get(id)); }
	/// Gets the corresponding type variable for the provided type variable ID.
	inline TypeVar& get_type_var(TypeVar::ID id) { return type_pool_.at(type_substs_.get(id)); }
	/// Gets the corresponding name variable for the provided name variable ID.
	inline NameVar const& get_name_var(NameVar::ID id) const {
		return name_pool_.at(const_cast<Resolver*>(this)->name_substs_.get(id));
	}
	/// Gets the corresponding type variable for the provided type variable ID.
	inline TypeVar const& get_type_var(TypeVar::ID id) const {
		return type_pool_.at(const_cast<Resolver*>(this)->type_substs_.get(id));
	}
	/// Returns the span for a given type ID.
	inline Span get_type_span(TypeVar::ID id) const { return std::get<0>(type_span_pool_.at(id)); }
	/// Returns the file ID for a given type ID.
	inline FileContext::ID get_type_file_id(TypeVar::ID id) const { return std::get<1>(type_span_pool_.at(id)); }

	/// Adds debug info for a type given its ID to the provided stream.
	std::ostream& debug_print(std::ostream&, TypeVar const&) const;
	/// Prints debug info for a type, without a newline.
	void debug_print(TypeVar const&) const;
	/// Adds debug info for a name given its ID to the provided stream.
	std::ostream& debug_print(std::ostream&, NameVar const&) const;
	/// Prints debug info for a name, without a newline.
	void debug_print(NameVar const&) const;
	/// Adds debug info for a type given its ID to the provided stream.
	std::ostream& debug_print_type(std::ostream&, TypeVar::ID) const;
	/// Prints debug info for a type given its ID, without a newline.
	void debug_print_type(TypeVar::ID) const;
	/// Adds debug info for a name given its ID to the provided stream.
	std::ostream& debug_print_name(std::ostream&, NameVar::ID) const;
	/// Prints debug info for a name given its ID, without a newline.
	void debug_print_name(NameVar::ID) const;

	/// Adds a name for a type suitable for a diagnostic to the provided stream.
	std::ostream& get_name(std::ostream&, TypeVar const&) const;
	/// Returns a name for a type suitable for a diagnostic.
	std::string get_name(TypeVar const&) const;
	/// Adds a name for a name suitable for a diagnostic to the provided stream.
	std::ostream& get_name(std::ostream&, NameVar const&) const;
	/// Returns a name for a name suitable for a diagnostic.
	std::string get_name(NameVar const&) const;
	/// Adds a name for a type suitable for a diagnostic to the provided stream.
	std::ostream& get_type_name(std::ostream&, TypeVar::ID) const;
	/// Returns a name for a type suitable for a diagnostic.
	std::string get_type_name(TypeVar::ID) const;
	/// Adds a name for a name suitable for a diagnostic to the provided stream.
	std::ostream& get_name_name(std::ostream&, NameVar::ID) const;
	/// Returns a name for a name suitable for a diagnostic.
	std::string get_name_name(NameVar::ID) const;

	/// Returns a type sample for the provided type ID.
	Diagnostic::Sample get_type_sample(TypeVar::ID, OutFmt::Color) const;

	void add_unknown_symbol_diagnostic(std::string_view symbol, Span, std::vector<std::string> const& possibilities, std::string_view scope_type, FileContext::ID, bool add_import_suggestion = false);

	TypeVar from_type(AST::Type::Atom const&, FileContext::ID);
	TypeVar from_type(AST::Type const&, FileContext::ID);

	/// Reconstructs an inferred type, throwing a diagnostic if it is still unknown. This should be used only for
	/// values and expressions, as it does not allow functions or modules to be values directly. It also takes in a
	/// type origin, which overrides the type ID for diagnostics.
	IR::Type reconstruct_type(TypeVar::ID type_id, TypeVar::ID type_origin, bool allow_functions = false);
	/// Reconstructs an inferred type, throwing a diagnostic if it is still unknown. This should be used only for
	/// values and expressions, as it does not allow functions or modules to be values directly.
	IR::Type reconstruct_type(TypeVar::ID, bool allow_functions = false);

	void infer();

	// === LOWERING ===

	/// Gets the default value for the given type.
	Spanned<IR::Value> lower_get_default_value(IR::Type const&, Span, FileContext::ID);

	/// Lowers an identifier into its IR equivalent and type.
	std::optional<std::tuple<Spanned<IR::Identifier>, IR::Type>> lower(Spanned<AST::Identifier> const&, bool allow_functions = false);
	/// Lowers an identifier into its IR equivalent and type.
	std::optional<std::tuple<IR::Identifier, IR::Type>> lower(AST::Identifier const&, bool allow_functions = false);
	/// Lowers a name.
	Spanned<IR::Identifier> lower_name(Spanned<AST::Name> const&);

	// FIXME: clang-format goes BALLISTIC over these function declarations
	// TODO: fix allow_functions
	// TODO: clean this mess up
	Spanned<std::tuple<IR::Identifier, IR::Type>> extract_value_id(AST::Expression const&, Span, std::vector<IR::BasicBlock>&, FileContext::ID, bool allow_functions = false);
	Spanned<IR::Identifier> extract_value_id(IR::Value&&, Span, IR::Type&&, TypeVar::ID, std::vector<IR::BasicBlock>&, FileContext::ID);
	Spanned<IR::Place> place_from_value(Spanned<IR::Value>&&, IR::Type&&, TypeVar::ID, std::vector<IR::BasicBlock>&, FileContext::ID);
	Spanned<IR::Value::Atom> extract_value(AST::Expression const&, Span, std::vector<IR::BasicBlock>&, FileContext::ID, bool allow_functions = false);
	Spanned<IR::Value::Atom> extract_value(Spanned<AST::Expression> const&, std::vector<IR::BasicBlock>&, FileContext::ID, bool allow_functions = false);
	IR::Value::Atom lower_atom(AST::Expression::Atom::StructLiteral const&, TypeVar::ID, Span, std::vector<IR::BasicBlock>&, FileContext::ID, bool allow_functions = false);
	IR::Value::Atom lower_atom(AST::Expression::Atom const&, TypeVar::ID, Span, std::vector<IR::BasicBlock>&, FileContext::ID, bool allow_functions = false);

	Spanned<IR::Value> lower_value(AST::Expression::AddressOperation const&, Span, std::vector<IR::BasicBlock>&, FileContext::ID, bool allow_functions = false);
	Spanned<IR::Value> lower_value(AST::Expression::FunctionCall&, Span, std::vector<IR::BasicBlock>&, FileContext::ID, bool allow_functions = false);
	Spanned<IR::Value> lower_value(AST::Expression::Atom const&, TypeVar::ID, Span, std::vector<IR::BasicBlock>&, FileContext::ID, bool allow_functions = false);
	Spanned<IR::Value> lower_value(AST::Expression const&, Span, std::vector<IR::BasicBlock>&, FileContext::ID, bool allow_functions = false);

	/// Lowers a value expression, that is, one found anywhere but the left hand side of a Set statement.
	Spanned<IR::Value> lower_value(Spanned<AST::Expression> const&, std::vector<IR::BasicBlock>&, FileContext::ID, bool allow_functions = false);

	Spanned<IR::Place> lower_place(AST::Expression::AddressOperation const&, TypeVar::ID, Span, std::vector<IR::BasicBlock>&, FileContext::ID, bool allow_functions = false);
	Spanned<IR::Place> lower_place(AST::Expression::FunctionCall const&, TypeVar::ID, Span, std::vector<IR::BasicBlock>&, FileContext::ID, bool allow_functions = false);
	Spanned<IR::Place> lower_place(AST::Expression::UnaryOperation const&, TypeVar::ID, Span, std::vector<IR::BasicBlock>&, FileContext::ID, bool allow_functions = false);
	Spanned<IR::Place> lower_place(AST::Expression::Atom const&, TypeVar::ID, Span, std::vector<IR::BasicBlock>&, FileContext::ID, bool allow_functions = false);
	Spanned<IR::Place> lower_place(AST::Expression::MemberAccess const&, TypeVar::ID, Span, std::vector<IR::BasicBlock>&, FileContext::ID, bool allow_functions = false);
	Spanned<IR::Place> lower_place(AST::Expression const&, Span, std::vector<IR::BasicBlock>&, FileContext::ID, bool allow_functions = false);

	/// Lowers a place expression, that is, one found at the left hand side of a Set statement.
	Spanned<IR::Place> lower_place(Spanned<AST::Expression> const&, std::vector<IR::BasicBlock>&, FileContext::ID, bool allow_functions = false);

	std::optional<Spanned<IR::Statement>> lower(AST::Statement::Declare const&, Span, std::vector<IR::BasicBlock>&, FileContext::ID);
	std::optional<Spanned<IR::Statement>> lower(AST::Statement::Set const&, Span, std::vector<IR::BasicBlock>&, FileContext::ID);
	std::optional<Spanned<IR::Statement>> lower(Spanned<AST::Statement> const&, AST::Function&, std::vector<IR::BasicBlock>&, FileContext::ID);
	void                                  lower(std::vector<Spanned<AST::Statement>> const&, AST::Function&, std::vector<IR::BasicBlock>&, FileContext::ID);

	IR::GenericDeclaration lower(std::optional<AST::GenericDeclaration>&, FileContext::ID);

	IR::Function lower(AST::Function&, FileContext::ID);
	IR::Struct   lower(AST::Struct&, FileContext::ID);
	IR::Module   lower(AST::Module&, FileContext::ID);

	/// Lowers all files into IR modules.
	std::vector<IR::Module> lower();
};
