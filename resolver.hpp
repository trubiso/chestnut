#pragma once
#include "ast/function.hpp"
#include "ast/identifier.hpp"
#include "ast/module.hpp"
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
		, module_table_ {}
		, counter_ {0} {}

	void resolve();

private:
	struct TypeInfo {
		typedef uint32_t ID;

		struct SameAs {
			ID id;
		};

		struct KnownInteger {
			// this may not be Any
			AST::Type::Atom::Integer integer;
		};

		struct KnownFloat {
			AST::Type::Atom::Float::Width width;
		};

		struct PartialInteger {
			// this may be Any
			AST::Type::Atom::Integer integer;
			bool                     signed_is_known;
		};

		struct PartialFloat {
			std::optional<AST::Type::Atom::Float::Width> width;
		};

		// TODO: register function argument types and return type

		enum class Kind {
			/// Completely unknown type.
			Unknown,
			/// Unifies with anything, to avoid throwing more errors than needed.
			Bottom,
			/// Represents a module. For now, it does not hold any extra information.
			Module,
			/// Represents a function. For now, it does not hold any extra information.
			Function,
			/// The exact same as another type.
			SameAs,
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
			/// Partially resolved float type.
			PartialFloat,
		};

		typedef std::variant<
			std::monostate,  // Unknown
			std::monostate,  // Bottom
			std::monostate,  // Module
			std::monostate,  // Function
			SameAs,          // SameAs
			std::monostate,  // KnownVoid
			std::monostate,  // KnownChar
			std::monostate,  // KnownBool
			KnownInteger,    // KnownInteger
			KnownFloat,      // KnownFloat
			PartialInteger,  // PartialInteger
			PartialFloat     // PartialFloat
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

		inline static TypeInfo make_function() {
			return TypeInfo(value_t {std::in_place_index<(size_t) Kind::Function>, std::monostate {}});
		}

		inline static TypeInfo make_same_as(ID id) {
			return TypeInfo(value_t {std::in_place_index<(size_t) Kind::SameAs>, SameAs {id}});
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

		inline static TypeInfo make_partial_float(PartialFloat&& partial_float) {
			return TypeInfo(
				value_t {std::in_place_index<(size_t) Kind::PartialFloat>, std::move(partial_float)}
			);
		}

		inline SameAs const& get_same_as() const { return std::get<(size_t) Kind::SameAs>(value); }

		inline KnownInteger const& get_known_integer() const {
			return std::get<(size_t) Kind::KnownInteger>(value);
		}

		inline KnownFloat const& get_known_float() const { return std::get<(size_t) Kind::KnownFloat>(value); }

		inline PartialInteger const& get_partial_integer() const {
			return std::get<(size_t) Kind::PartialInteger>(value);
		}

		inline PartialFloat const& get_partial_float() const {
			return std::get<(size_t) Kind::PartialFloat>(value);
		}

		static TypeInfo from_type(AST::Type::Atom const&);
		static TypeInfo from_type(AST::Type const&);
	};

	std::vector<TypeInfo> type_pool_;

	TypeInfo::ID type_counter_ = 0;

	/// Returns a new ID produced by the type counter.
	TypeInfo::ID type_next();

	/// Registers a type in the type pool and returns its ID.
	TypeInfo::ID register_type(TypeInfo&&);

	struct Symbol {
		AST::SymbolID   id;
		FileContext::ID file_id;
		Span            span;
		std::string     name;

		std::variant<AST::Module*, AST::Function*, std::monostate> item;

		TypeInfo::ID type;

		bool mutable_;
	};

	std::vector<Symbol> symbol_pool_;

	inline Symbol& get_single_symbol(AST::SymbolID id) { return symbol_pool_.at(id); }

	inline Symbol& get_single_symbol(AST::Identifier const& identifier) {
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

	void add_unknown_symbol_diagnostic(
		std::string_view                symbol,
		Span                            span,
		std::vector<std::string> const& possible_symbols,
		std::string_view                scope_type,
		FileContext::ID
	);

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
