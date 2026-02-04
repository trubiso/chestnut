#pragma once
#include "ast/identifier.hpp"
#include "diagnostic.hpp"

#include <variant>
#include <vector>

namespace IR {

// all identifiers are now symbols
using Identifier = AST::SymbolID;

struct Type {
	struct Atom {
		// TODO: eventually support types which are identifiers. for now, we won't, as there is no way to create
		// them.

		class Integer {
			uint32_t width;
			bool signed_;  // TODO: maybe express this as a bit in width, although that's kinda ridiculous

			// all integers must now know their size
			static uint32_t const PTR  = (1 << 23) + 1;
			static uint32_t const SIZE = (1 << 23) + 2;

			explicit Integer(uint32_t width, bool signed_) : width {width}, signed_ {signed_} {}

			static inline constexpr bool is_valid_user_width(uint32_t w) { return w < (1 << 23); }

			static inline constexpr bool is_valid_width(uint32_t w) {
				return is_valid_user_width(w) || w == PTR || w == SIZE;
			}

		public:
			enum class WidthType {
				Fixed,
				Ptr,
				Size,
			};

			static inline std::optional<Integer> with_width(uint32_t width, bool signed_) {
				if (!is_valid_width(width)) return {};
				else return Integer {width, signed_};
			}

			static inline Integer ptr(bool signed_) { return Integer {PTR, signed_}; }

			static inline Integer size(bool signed_) { return Integer {SIZE, signed_}; }

			inline WidthType width_type() const {
				switch (width) {
				case PTR:  return WidthType::Ptr;
				case SIZE: return WidthType::Size;
				default:   return WidthType::Fixed;
				}
			}

			inline bool is_signed() const { return signed_; }

			inline std::optional<uint32_t> bit_width() const {
				return width_type() == WidthType::Fixed ? std::optional {width} : std::nullopt;
			}
		};

		struct Float {
			enum class Width { F16, F32, F64, F128 } width;

			inline constexpr uint8_t width_value() const {
				switch (width) {
				case Width::F16:  return 16;
				case Width::F32:  return 32;
				case Width::F64:  return 64;
				case Width::F128: return 128;
				}
				[[assume(false)]];
			}
		};

		enum class Kind { Integer, Float, Void, Char, Bool, Error };

		typedef std::variant<Integer, Float, std::monostate, std::monostate, std::monostate, std::monostate>
			value_t;

		value_t value;

		inline constexpr Kind kind() const { return (Kind) value.index(); }

		inline static Atom make_integer(Integer&& integer) {
			return Atom(value_t {std::in_place_index<(size_t) Kind::Integer>, integer});
		}

		inline static Atom make_float(Float::Width width) {
			return Atom(value_t {std::in_place_index<(size_t) Kind::Float>, Float {width}});
		}

		inline static Atom make_void() {
			return Atom(value_t {std::in_place_index<(size_t) Kind::Void>, std::monostate {}});
		}

		inline static Atom make_char() {
			return Atom(value_t {std::in_place_index<(size_t) Kind::Char>, std::monostate {}});
		}

		inline static Atom make_bool() {
			return Atom(value_t {std::in_place_index<(size_t) Kind::Bool>, std::monostate {}});
		}

		inline static Atom make_error() {
			return Atom(value_t {std::in_place_index<(size_t) Kind::Error>, std::monostate {}});
		}

		inline Integer const& get_integer() const { return std::get<(size_t) Kind::Integer>(value); }

		inline Float get_float() const { return std::get<(size_t) Kind::Float>(value); }
	};

	enum class Kind { Atom };

	typedef std::variant<Atom> value_t;

	value_t value;

	inline constexpr Kind kind() const { return (Kind) value.index(); }

	inline static Type make_atom(Atom&& atom) {
		return Type(value_t {std::in_place_index<(size_t) Kind::Atom>, atom});
	}

	inline Atom const& get_atom() const { return std::get<(size_t) Kind::Atom>(value); }
};

std::ostream& operator<<(std::ostream&, Type::Atom const&);
std::ostream& operator<<(std::ostream&, Type const&);

struct Expression {
	// atoms must now be either variables or literals
	struct Atom {
		enum class Kind {
			Identifier,
			Literal,
		};

		struct Literal {
			enum class Kind { Number, String, Char } kind;
			std::string literal;
		};

		typedef std::variant<Identifier, Literal> value_t;

		value_t value;

		inline constexpr Kind kind() const { return (Kind) value.index(); }

		inline static Atom make_identifier(Identifier identifier) {
			return Atom(value_t {std::in_place_index<(size_t) Kind::Identifier>, identifier});
		}

		inline static Atom make_literal(Literal::Kind kind, std::string literal) {
			return Atom(
				value_t {
					std::in_place_index<(size_t) Kind::Literal>,
					Literal {kind, literal}
                        }
			);
		}

		inline Identifier get_identifier() const { return std::get<(size_t) Kind::Identifier>(value); }

		inline Identifier& get_identifier() { return std::get<(size_t) Kind::Identifier>(value); }

		inline Literal const& get_literal() const { return std::get<(size_t) Kind::Literal>(value); }
	};

	struct FunctionCall {
		// we need to know what we're calling directly
		Spanned<Identifier> callee;
		// labeled arguments are mere syntactic sugar
		std::vector<Spanned<Atom>> arguments;
	};

	// expressions must now be either atoms or function calls
	enum class Kind { Atom, FunctionCall };

	typedef std::variant<Atom, FunctionCall> value_t;

	value_t value;

	inline constexpr Kind kind() const { return (Kind) value.index(); }

	inline static Expression make_atom(Atom&& atom) {
		return Expression(value_t {std::in_place_index<(size_t) Kind::Atom>, std::move(atom)});
	}

	inline static Expression
	make_function_call(Spanned<Identifier>&& callee, std::vector<Spanned<Atom>>&& arguments) {
		return Expression(
			value_t {
				std::in_place_index<(size_t) Kind::FunctionCall>,
				FunctionCall {std::move(callee), std::move(arguments)}
                }
		);
	}

	inline Atom const& get_atom() const { return std::get<(size_t) Kind::Atom>(value); }

	inline Atom& get_atom() { return std::get<(size_t) Kind::Atom>(value); }

	inline FunctionCall const& get_function_call() const { return std::get<(size_t) Kind::FunctionCall>(value); }

	inline FunctionCall& get_function_call() { return std::get<(size_t) Kind::FunctionCall>(value); }
};

std::ostream& operator<<(std::ostream&, Expression::Atom::Literal const&);
std::ostream& operator<<(std::ostream&, Expression::Atom const&);
std::ostream& operator<<(std::ostream&, Expression::FunctionCall const&);
std::ostream& operator<<(std::ostream&, Expression const&);

struct Statement;

typedef std::vector<Spanned<Statement>> Scope;

struct Statement {
	struct Declare {
		Spanned<Identifier>                name;
		Type                               type;
		std::optional<Spanned<Expression>> value;

		Spanned<bool> mutable_;
	};

	// set statements must set an identifier now. once we do deref sets, that will be a separate statement anyways.
	struct Set {
		Spanned<Identifier> name;
		Spanned<Expression> value;
	};

	struct Return {
		std::optional<Spanned<Expression>> value;
	};

	// expression statements are now calls and scope statements are resolved now anyways
	enum class Kind { Declare, Set, Call, Return };

	typedef std::variant<Declare, Set, Expression::FunctionCall, Return> value_t;

	value_t value;

	inline constexpr Kind kind() const { return (Kind) value.index(); }

	inline static Statement make_declare(Declare&& declare) {
		return Statement(value_t {std::in_place_index<(size_t) Kind::Declare>, std::move(declare)});
	}

	inline static Statement make_set(Set&& set) {
		return Statement(value_t {std::in_place_index<(size_t) Kind::Set>, std::move(set)});
	}

	inline static Statement make_call(Expression::FunctionCall&& call) {
		return Statement(value_t {std::in_place_index<(size_t) Kind::Call>, std::move(call)});
	}

	inline static Statement make_return(Return&& return_) {
		return Statement(value_t {std::in_place_index<(size_t) Kind::Return>, std::move(return_)});
	}

	inline Declare const& get_declare() const { return std::get<(size_t) Kind::Declare>(value); }

	inline Declare& get_declare() { return std::get<(size_t) Kind::Declare>(value); }

	inline Set const& get_set() const { return std::get<(size_t) Kind::Set>(value); }

	inline Set& get_set() { return std::get<(size_t) Kind::Set>(value); }

	inline Expression::FunctionCall const& get_call() const { return std::get<(size_t) Kind::Call>(value); }

	inline Expression::FunctionCall& get_call() { return std::get<(size_t) Kind::Call>(value); }

	inline Return const& get_return() const { return std::get<(size_t) Kind::Return>(value); }

	inline Return& get_return() { return std::get<(size_t) Kind::Return>(value); }
};

std::ostream& operator<<(std::ostream&, Statement::Declare const&);
std::ostream& operator<<(std::ostream&, Statement::Set const&);
std::ostream& operator<<(std::ostream&, Statement::Return const&);
std::ostream& operator<<(std::ostream&, Statement const&);

struct Function {
	struct Argument {
		Spanned<Identifier> name;
		Spanned<Type>       type;
	};

	Spanned<Identifier>   name;
	std::vector<Argument> arguments;
	Spanned<Type>         return_type;

	std::optional<Scope> body;
};

std::ostream& operator<<(std::ostream&, Function const&);

struct Module {
	Spanned<Identifier>     name;
	std::vector<Identifier> items;
};

// TODO: we don't need all of these spans, because most of them are contained within the symbol table already
struct Symbol {
	AST::SymbolID   id;
	FileContext::ID file_id;
	Span            span;
	std::string     name;

	/// Holds the module item that this points to, if any.
	std::variant<Module, Function, std::monostate> item;

	/// Whether this symbol can be mutated (should be true only for mutable declarations and arguments).
	bool mutable_;
};

}  // namespace IR
