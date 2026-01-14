#include "lexer.hpp"
#include "span.hpp"
#include "stream.hpp"
#include "token.hpp"

#include <functional>
#include <memory>
#include <optional>
#include <string_view>
#include <variant>
#include <vector>

namespace AST {

// TODO: create a StringInterner to avoid moving around string views constantly

// this is a superset of unqualified identifiers, as .absolute = false, .path = [<id>] is an unqualified identifier
struct QualifiedIdentifier {
	bool absolute;
	// FIXME: this should absolutely be a SmallVec to avoid heap allocs
	std::vector<Spanned<std::string_view>> path;
};

std::ostream& operator<<(std::ostream&, QualifiedIdentifier const&);

struct Expression {
	struct Atom {
		// TODO: add parenthesized expression to atom
		enum class Kind {
			Identifier    = 0,
			NumberLiteral = 1,
			StringLiteral = 2,
			CharLiteral   = 3,
		};

		struct NumberLiteral {
			std::string_view                literal;
			std::optional<std::string_view> suffix;
		};

		struct StringLiteral {
			std::string_view                literal;
			std::optional<std::string_view> suffix;
		};

		struct CharLiteral {
			char                            literal;
			std::optional<std::string_view> suffix;
		};

		typedef std::variant<QualifiedIdentifier, NumberLiteral, StringLiteral, CharLiteral> value_t;

		value_t value;

		inline static Atom make_identifier(QualifiedIdentifier&& identifier) {
			return Atom(value_t {std::in_place_index<(size_t) Kind::Identifier>, identifier});
		}

		inline static Atom
		make_number_literal(std::string_view literal, std::optional<std::string_view> suffix) {
			return Atom(
				value_t {
					std::in_place_index<(size_t) Kind::NumberLiteral>,
					NumberLiteral {literal, suffix}
                        }
			);
		}

		inline static Atom
		make_string_literal(std::string_view literal, std::optional<std::string_view> suffix) {
			return Atom(
				value_t {
					std::in_place_index<(size_t) Kind::StringLiteral>,
					StringLiteral {literal, suffix}
                        }
			);
		}

		inline static Atom make_char_literal(char literal, std::optional<std::string_view> suffix) {
			return Atom(
				value_t {
					std::in_place_index<(size_t) Kind::CharLiteral>,
					CharLiteral {literal, suffix}
                        }
			);
		}

		inline QualifiedIdentifier const& get_identifier() const { return std::get<(size_t) Kind::Identifier>(value); }

		inline NumberLiteral const& get_number_literal() const {
			return std::get<(size_t) Kind::NumberLiteral>(value);
		}

		inline StringLiteral const& get_string_literal() const {
			return std::get<(size_t) Kind::StringLiteral>(value);
		}

		inline CharLiteral const& get_char_literal() const { return std::get<(size_t) Kind::CharLiteral>(value); }
	};

	struct UnaryOperation {
		std::unique_ptr<Spanned<Expression>> operand;
		Token::Symbol                        operation;
	};

	struct BinaryOperation {
		std::unique_ptr<Spanned<Expression>> lhs;
		std::unique_ptr<Spanned<Expression>> rhs;
		Token::Symbol                        operation;
	};

	enum class Kind {
		Atom            = 0,
		UnaryOperation  = 1,
		BinaryOperation = 2,
	};

	typedef std::variant<Atom, UnaryOperation, BinaryOperation> value_t;

	value_t value;

	inline static Expression make_atom(Atom&& atom) {
		return Expression(value_t {std::in_place_index<(size_t) Kind::Atom>, atom});
	}

	inline static Expression
	make_unary_operation(std::unique_ptr<Spanned<Expression>>&& operand, Token::Symbol operation) {
		return Expression(
			value_t {
				std::in_place_index<(size_t) Kind::UnaryOperation>,
				UnaryOperation {std::move(operand), operation}
                }
		);
	}

	inline static Expression make_binary_operation(
		std::unique_ptr<Spanned<Expression>>&& lhs,
		std::unique_ptr<Spanned<Expression>>&& rhs,
		Token::Symbol                          operation
	) {
		return Expression(
			value_t {
				std::in_place_index<(size_t) Kind::BinaryOperation>,
				BinaryOperation {std::move(lhs), std::move(rhs), operation}
                }
		);
	}

	inline Atom const& get_atom() const { return std::get<(size_t) Kind::Atom>(value); }

	inline UnaryOperation const& get_unary_operation() const {
		return std::get<(size_t) Kind::UnaryOperation>(value);
	}

	inline BinaryOperation const& get_binary_operation() const {
		return std::get<(size_t) Kind::BinaryOperation>(value);
	}
};

std::ostream& operator<<(std::ostream&, Expression::Atom::NumberLiteral const&);
std::ostream& operator<<(std::ostream&, Expression::Atom::StringLiteral const&);
std::ostream& operator<<(std::ostream&, Expression::Atom::CharLiteral const&);
std::ostream& operator<<(std::ostream&, Expression::Atom const&);
std::ostream& operator<<(std::ostream&, Expression::UnaryOperation const&);
std::ostream& operator<<(std::ostream&, Expression::BinaryOperation const&);
std::ostream& operator<<(std::ostream&, Expression const&);

struct Type {
	// TODO: eventually support types which are identifiers. for now, we won't, as there is no way to create them.

	enum class Kind {
		Integer = 0,
		Float   = 1,
		Void    = 2,
		Char    = 3,
		Bool    = 4,
	};

	class Integer {
		uint32_t width;
		bool     signed_;  // TODO: maybe express this as a bit in width, although that's kinda ridiculous

		static uint32_t const ANY  = (1 << 23) + 1;
		static uint32_t const PTR  = (1 << 23) + 2;
		static uint32_t const SIZE = (1 << 23) + 3;

		explicit Integer(uint32_t width, bool signed_) : width {width}, signed_ {signed_} {}

		static inline constexpr bool is_valid_user_width(uint32_t w) { return w < (1 << 23); }

		static inline constexpr bool is_valid_width(uint32_t w) {
			return is_valid_user_width(w) || w == ANY || w == PTR || w == SIZE;
		}

	public:
		enum class WidthType {
			Fixed,
			Any,
			Ptr,
			Size,
		};

		static inline std::optional<Integer> with_width(uint32_t width, bool signed_) {
			if (!is_valid_width(width)) return {};
			else return Integer {width, signed_};
		}

		static inline Integer any(bool signed_) { return Integer {ANY, signed_}; }

		static inline Integer ptr(bool signed_) { return Integer {PTR, signed_}; }

		static inline Integer size(bool signed_) { return Integer {SIZE, signed_}; }

		inline WidthType width_type() const {
			switch (width) {
			case ANY:  return WidthType::Any;
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
		}
	};

	typedef std::variant<Integer, Float, std::monostate, std::monostate, std::monostate> value_t;

	value_t value;

	// NOTE: would be beautiful to codegen all of this boilerplate, but alas,

	inline static Type make_integer(Integer&& integer) {
		return Type(value_t {std::in_place_index<(size_t) Kind::Integer>, integer});
	}

	inline static Type make_float(Float::Width width) {
		return Type(value_t {std::in_place_index<(size_t) Kind::Float>, Float {width}});
	}

	inline static Type make_void() {
		return Type(value_t {std::in_place_index<(size_t) Kind::Void>, std::monostate {}});
	}

	inline static Type make_char() {
		return Type(value_t {std::in_place_index<(size_t) Kind::Char>, std::monostate {}});
	}

	inline static Type make_bool() {
		return Type(value_t {std::in_place_index<(size_t) Kind::Bool>, std::monostate {}});
	}

	inline Integer get_integer() const { return std::get<(size_t) Kind::Integer>(value); }

	inline Float get_float() const { return std::get<(size_t) Kind::Float>(value); }
};

std::ostream& operator<<(std::ostream&, Type const&);

struct Function {
	struct Argument {
		Spanned<std::string_view> name;
		Spanned<Type>             type;
	};

	Spanned<std::string_view>    name;
	std::vector<Argument>        arguments;
	std::optional<Spanned<Type>> return_type;

	// TODO: body
};

struct Module {
	std::optional<Spanned<std::string_view>> name;

	// we cannot make this a struct, because C++ does not allow incomplete types in variants (which is fair, but we
	// know this will be on the heap anyways).
	using Item = std::variant<Function, Module>;

	struct Body {
		std::vector<Item> items;
	} body;
};

struct Node {
	Span                 span;
	std::variant<Module> data;
};

class Parser {
public:
	explicit Parser(Stream<Token>&& tokens) : tokens_(tokens) {}

	bool advance();

	inline std::vector<Diagnostic>& diagnostics() { return diagnostics_; }

private:
	enum class Keyword {
		Import,
		Module,
		Const,
		Mut,
		Func,
		Return,
	};

	Stream<Token>           tokens_;
	std::vector<Diagnostic> diagnostics_;

	template <typename T>
	inline std::optional<Spanned<T>> spanned(std::function<std::optional<T>()> function) {
		if (!tokens_.peek().has_value()) return {};
		size_t begin = tokens_.peek().value().begin;

		std::optional<T> value = function();
		if (!value.has_value()) return {};

		size_t end = tokens_.peek().value_or(tokens_.last()).begin;

		return Spanned<T> {Span(begin, end), std::move(value.value())};
	}

	// consume_ methods return a non-null/true value if they found a token
	// abiding by the specified criteria and increment the index
	bool consume_keyword(Keyword);
	bool consume_symbol(Token::Symbol);

	std::optional<std::string_view> consume_number_literal();
	std::optional<std::string_view> consume_string_literal();
	std::optional<char>             consume_char_literal();

	std::optional<std::string_view>    consume_identifier();
	std::optional<std::string_view>    consume_tag();
	std::optional<QualifiedIdentifier> consume_qualified_identifier();

	std::optional<Type> consume_type();

	std::optional<Expression::Atom> consume_expression_atom();
	std::optional<Expression>       consume_expression_unary_l1();
	std::optional<Expression>       consume_expression_binop_l1();
	std::optional<Expression>       consume_expression();

	// peek_ methods do not increment the index.
	bool peek_symbol(Token::Symbol) const;
	bool peek_keyword(Keyword) const;

	// expect_ methods do the same as consume_, but throw a diagnostic as well
	// upon failure. The reason string is only copied if a diagnostic is thrown.
	bool expect_symbol(std::string_view reason, Token::Symbol);

	std::optional<std::string_view> expect_identifier(std::string_view reason);

	std::optional<Type> expect_type(std::string_view reason);

	std::optional<Expression::Atom> expect_expression_atom(std::string_view reason);
	std::optional<Expression>       expect_expression_unary_l1(std::string_view reason);
	std::optional<Expression>       expect_expression_binop_l1(std::string_view reason);
	std::optional<Expression>       expect_expression(std::string_view reason);

	// skip semicolons
	void skip_semis();

	std::optional<Module>       parse_module();
	std::optional<Module::Item> parse_module_item();
	std::optional<Module::Body> parse_module_body(bool bare = false);

	std::optional<Function> parse_function();
};

}  // namespace AST
