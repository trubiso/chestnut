#pragma once
#include "ast/identifier.hpp"
#include "diagnostic.hpp"

#include <memory>
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

		enum class Kind { Integer, Float, Void, Char, Bool, Named, Error };

		typedef std::variant<
			Integer,
			Float,
			std::monostate,
			std::monostate,
			std::monostate,
			Identifier,
			std::monostate>
			value_t;

		value_t value;

		inline constexpr Kind kind() const { return (Kind) value.index(); }

		inline bool is_integer() const { return kind() == Kind::Integer; }

		inline bool is_float() const { return kind() == Kind::Float; }

		inline bool is_void() const { return kind() == Kind::Void; }

		inline bool is_char() const { return kind() == Kind::Char; }

		inline bool is_bool() const { return kind() == Kind::Bool; }

		inline bool is_named() const { return kind() == Kind::Named; }

		inline bool is_error() const { return kind() == Kind::Error; }

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

		inline static Atom make_named(Identifier id) {
			return Atom(value_t {std::in_place_index<(size_t) Kind::Named>, id});
		}

		inline static Atom make_error() {
			return Atom(value_t {std::in_place_index<(size_t) Kind::Error>, std::monostate {}});
		}

		inline Integer const& get_integer() const { return std::get<(size_t) Kind::Integer>(value); }

		inline Float get_float() const { return std::get<(size_t) Kind::Float>(value); }

		inline Identifier const& get_named() const { return std::get<(size_t) Kind::Named>(value); }
	};

	struct Pointer {
		std::unique_ptr<Spanned<Type>> type;
		bool                           mutable_;
	};

	enum class Kind { Atom, Pointer };

	typedef std::variant<Atom, Pointer> value_t;

	value_t value;

	inline constexpr Kind kind() const { return (Kind) value.index(); }

	inline static Type make_atom(Atom&& atom) {
		return Type(value_t {std::in_place_index<(size_t) Kind::Atom>, atom});
	}

	inline static Type make_pointer(Pointer&& pointer) {
		return Type(value_t {std::in_place_index<(size_t) Kind::Pointer>, std::move(pointer)});
	}

	inline bool is_atom() const { return kind() == Kind::Atom; }

	inline bool is_pointer() const { return kind() == Kind::Pointer; }

	inline Atom const& get_atom() const { return std::get<(size_t) Kind::Atom>(value); }

	inline Pointer const& get_pointer() const { return std::get<(size_t) Kind::Pointer>(value); }

	Type clone() const;
};

uint32_t const                 DEFAULT_INTEGER_WIDTH = 32;
Type::Atom::Float::Width const DEFAULT_FLOAT_WIDTH   = Type::Atom::Float::Width::F32;

std::ostream& operator<<(std::ostream&, Type::Atom const&);
std::ostream& operator<<(std::ostream&, Type::Pointer const&);
std::ostream& operator<<(std::ostream&, Type const&);

// TODO: deal with the partial ordering of floats
enum class BuiltInFunction {
	AddUIntegers,
	AddSIntegers,
	AddFloats,
	SubtractUIntegers,
	SubtractSIntegers,
	SubtractFloats,
	MultiplyUIntegers,
	MultiplySIntegers,
	MultiplyFloats,
	DivideUIntegers,
	DivideSIntegers,
	DivideFloats,
	NegateSInteger,
	NegateFloat,
	NegateBool,
	EqIntegers,
	EqFloats,
	EqChars,
	EqBools,
	NeIntegers,
	NeFloats,
	NeChars,
	NeBools,
	GtUIntegers,
	GtSIntegers,
	GtFloats,
	GeUIntegers,
	GeSIntegers,
	GeFloats,
	LtUIntegers,
	LtSIntegers,
	LtFloats,
	LeUIntegers,
	LeSIntegers,
	LeFloats,
};

std::ostream& operator<<(std::ostream&, BuiltInFunction const&);

/// Represents a compound expression pointing to a place in memory, that is, an lvalue.
struct Place {
	enum class Kind { Symbol, Deref, Access, Error };

	struct Deref {
		std::unique_ptr<Spanned<Place>> address;
	};

	struct Access {
		std::unique_ptr<Spanned<Place>> accessee;
		size_t                          field_index;
	};

	typedef std::variant<Identifier, Deref, Access, std::monostate> value_t;

	value_t value;
	Type    type;

	inline constexpr Kind kind() const { return (Kind) value.index(); }

	inline static Place make_symbol(Identifier identifier, Type&& type) {
		return Place {identifier, std::move(type)};
	}

	inline static Place make_deref(std::unique_ptr<Spanned<Place>>&& address, Type&& type) {
		return Place {Deref {std::move(address)}, std::move(type)};
	}

	inline static Place make_access(std::unique_ptr<Spanned<Place>>&& accessee, size_t field_index, Type&& type) {
		return Place {
			Access {std::move(accessee), field_index},
			std::move(type)
		};
	}

	inline static Place make_error(Type&& type) { return Place {std::monostate {}, std::move(type)}; }

	inline bool is_symbol() const { return kind() == Kind::Symbol; }

	inline bool is_deref() const { return kind() == Kind::Deref; }

	inline bool is_access() const { return kind() == Kind::Access; }

	inline bool is_error() const { return kind() == Kind::Error; }

	inline Identifier const& get_symbol() const { return std::get<(size_t) Kind::Symbol>(value); }

	inline Identifier& get_symbol() { return std::get<(size_t) Kind::Symbol>(value); }

	inline Deref const& get_deref() const { return std::get<(size_t) Kind::Deref>(value); }

	inline Deref& get_deref() { return std::get<(size_t) Kind::Deref>(value); }

	inline Access const& get_access() const { return std::get<(size_t) Kind::Access>(value); }

	inline Access& get_access() { return std::get<(size_t) Kind::Access>(value); }
};

std::ostream& operator<<(std::ostream&, Place::Deref const&);
std::ostream& operator<<(std::ostream&, Place::Access const&);
std::ostream& operator<<(std::ostream&, Place const&);

struct Expression {
	// atoms must now be either variables or literals
	struct Atom {
		enum class Kind {
			Identifier,
			Literal,
			Bool,
			StructLiteral,
			Error,
		};

		struct Literal {
			enum class Kind { Number, String, Char } kind;
			std::string literal;
		};

		struct StructLiteral {
			Spanned<Identifier> name;
			// fields ordered according to the struct
			std::vector<Spanned<Atom>> fields;
		};

		typedef std::variant<Identifier, Literal, bool, StructLiteral, std::monostate> value_t;

		value_t  value;
		IR::Type type;

		inline constexpr Kind kind() const { return (Kind) value.index(); }

		inline static Atom make_identifier(Identifier identifier, IR::Type type) {
			return Atom(
				value_t {std::in_place_index<(size_t) Kind::Identifier>, identifier},
				std::move(type)
			);
		}

		inline static Atom make_literal(Literal::Kind kind, std::string literal, IR::Type type) {
			return Atom(
				value_t {
					std::in_place_index<(size_t) Kind::Literal>,
					Literal {kind, literal}
                        },
				std::move(type)
			);
		}

		// TODO: the type should always be bool, idk if it's even worth having it as an arg
		inline static Atom make_bool(bool value, IR::Type type) { return Atom {value, std::move(type)}; }

		inline static Atom
		make_struct_literal(Spanned<Identifier>&& name, std::vector<Spanned<Atom>>&& fields, IR::Type&& type) {
			return Atom {
				Atom::StructLiteral {std::move(name), std::move(fields)},
				std::move(type)
			};
		}

		inline static Atom make_error() {
			return Atom {std::monostate {}, Type::make_atom(Type::Atom::make_error())};
		}

		inline bool is_identifier() const { return kind() == Kind::Identifier; }

		inline bool is_literal() const { return kind() == Kind::Literal; }

		inline bool is_bool() const { return kind() == Kind::Bool; }

		inline bool is_struct_literal() const { return kind() == Kind::StructLiteral; }

		inline bool is_error() const { return kind() == Kind::Error; }

		inline Identifier get_identifier() const { return std::get<(size_t) Kind::Identifier>(value); }

		inline Identifier& get_identifier() { return std::get<(size_t) Kind::Identifier>(value); }

		inline Literal const& get_literal() const { return std::get<(size_t) Kind::Literal>(value); }

		inline StructLiteral const& get_struct_literal() const {
			return std::get<(size_t) Kind::StructLiteral>(value);
		}

		inline bool get_bool() const { return std::get<(size_t) Kind::Bool>(value); }

		Atom clone() const;
	};

	struct FunctionCall {
		// we need to know what we're calling directly
		std::variant<Spanned<Identifier>, BuiltInFunction> callee;
		// labeled arguments are mere syntactic sugar
		std::vector<Spanned<Atom>> arguments;
	};

	struct Ref {
		Spanned<Place> value;
		bool           mutable_;
	};

	struct Load {
		Spanned<Place> value;
	};

	// expressions must now be either atoms or function calls
	enum class Kind { Atom, FunctionCall, Ref, Load };

	typedef std::variant<Atom, FunctionCall, Ref, Load> value_t;

	value_t value;

	inline constexpr Kind kind() const { return (Kind) value.index(); }

	inline static Expression make_atom(Atom&& atom) {
		return Expression(value_t {std::in_place_index<(size_t) Kind::Atom>, std::move(atom)});
	}

	inline static Expression make_function_call(
		std::variant<Spanned<Identifier>, BuiltInFunction>&& callee,
		std::vector<Spanned<Atom>>&&                         arguments
	) {
		return Expression(
			value_t {
				std::in_place_index<(size_t) Kind::FunctionCall>,
				FunctionCall {std::move(callee), std::move(arguments)}
                }
		);
	}

	inline static Expression make_ref(Spanned<Place>&& value, bool mutable_) {
		return Expression(
			value_t {
				std::in_place_index<(size_t) Kind::Ref>,
				Ref {std::move(value), mutable_}
                }
		);
	}

	inline static Expression make_load(Spanned<Place>&& place) {
		return Expression(value_t {std::in_place_index<(size_t) Kind::Load>, Load {std::move(place)}});
	}

	inline bool is_atom() const { return kind() == Kind::Atom; }

	inline bool is_function_call() const { return kind() == Kind::FunctionCall; }

	inline bool is_ref() const { return kind() == Kind::Ref; }

	inline bool is_load() const { return kind() == Kind::Load; }

	inline Atom const& get_atom() const { return std::get<(size_t) Kind::Atom>(value); }

	inline Atom& get_atom() { return std::get<(size_t) Kind::Atom>(value); }

	inline FunctionCall const& get_function_call() const { return std::get<(size_t) Kind::FunctionCall>(value); }

	inline FunctionCall& get_function_call() { return std::get<(size_t) Kind::FunctionCall>(value); }

	inline Ref const& get_ref() const { return std::get<(size_t) Kind::Ref>(value); }

	inline Ref& get_ref() { return std::get<(size_t) Kind::Ref>(value); }

	inline Load const& get_load() const { return std::get<(size_t) Kind::Load>(value); }

	inline Load& get_load() { return std::get<(size_t) Kind::Load>(value); }
};

std::ostream& operator<<(std::ostream&, Expression::Atom::Literal const&);
std::ostream& operator<<(std::ostream&, Expression::Atom::StructLiteral const&);
std::ostream& operator<<(std::ostream&, Expression::Atom const&);
std::ostream& operator<<(std::ostream&, Expression::FunctionCall const&);
std::ostream& operator<<(std::ostream&, Expression::Ref const&);
std::ostream& operator<<(std::ostream&, Expression::Load const&);
std::ostream& operator<<(std::ostream&, Expression const&);

struct Statement;

typedef std::vector<Spanned<Statement>> Scope;

struct Statement {
	// declaring a variable
	struct Declare {
		Spanned<Identifier> name;
		Type                type;
		Spanned<bool>       mutable_;
	};

	// setting a place in memory
	struct Set {
		Spanned<Place>      place;
		Spanned<Expression> value;
	};

	// expression statements are now calls and scope statements are resolved now anyways
	enum class Kind { Declare, Set, Call };

	typedef std::variant<Declare, Set, Expression::FunctionCall> value_t;

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

	inline Declare const& get_declare() const { return std::get<(size_t) Kind::Declare>(value); }

	inline Declare& get_declare() { return std::get<(size_t) Kind::Declare>(value); }

	inline Set const& get_set() const { return std::get<(size_t) Kind::Set>(value); }

	inline Set& get_set() { return std::get<(size_t) Kind::Set>(value); }

	inline Expression::FunctionCall const& get_call() const { return std::get<(size_t) Kind::Call>(value); }

	inline Expression::FunctionCall& get_call() { return std::get<(size_t) Kind::Call>(value); }
};

std::ostream& operator<<(std::ostream&, Statement::Declare const&);
std::ostream& operator<<(std::ostream&, Statement::Set const&);
std::ostream& operator<<(std::ostream&, Statement const&);

struct BasicBlock {
	typedef uint32_t ID;

	ID id;

	std::vector<Spanned<Statement>> statements;

	struct Goto {
		ID id;
	};

	struct Branch {
		Spanned<Expression::Atom> condition;

		ID true_;
		ID false_;
	};

	struct Return {
		std::optional<Spanned<Expression::Atom>> value;
	};

	std::variant<Goto, Branch, Return, std::monostate> jump;
};

std::ostream& operator<<(std::ostream&, BasicBlock const&);

struct Function {
	struct Argument {
		Spanned<Identifier> name;
		Spanned<Type>       type;
	};

	Spanned<Identifier>   name;
	std::vector<Argument> arguments;
	Spanned<Type>         return_type;

	std::vector<BasicBlock> body;

	bool extern_;

	BasicBlock& find_block(BasicBlock::ID);
};

std::ostream& operator<<(std::ostream&, Function const&);

struct Struct {
	Spanned<Identifier> name;

	struct Field {
		Spanned<std::string> name;
		Spanned<Type>        type;
	};

	std::vector<Field> fields;
};

std::ostream& operator<<(std::ostream&, Struct const&);

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
	std::variant<Module, Function, BuiltInFunction, Struct, std::monostate> item;

	/// Whether this symbol can be mutated (should be true only for mutable declarations and arguments).
	bool mutable_;
};

}  // namespace IR
