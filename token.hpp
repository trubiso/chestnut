#pragma once
#include <cstddef>
#include <ostream>
#include <string_view>
#include <utility>
#include <variant>

struct Token {
	enum class Symbol {
		// Operators
		Plus,
		Minus,
		Star,
		Div,
		Amp,
		Bar,
		Xor,
		Tilde,
		Eq,
		EqEq,
		Lt,
		Le,
		Gt,
		Ge,
		LtGt,
		Ne,
		Bang,
		Maybe,
		MaybeMaybe,
		MaybeDot,
		Percent,
		At,
		Dot,
		DotDot,
		DotDotLt,
		DotDotEq,
		LeftArrow,

		// punctuation
		LParen,
		RParen,
		LBracket,
		RBracket,
		LBrace,
		RBrace,

		Comma,
		Colon,
		ColonColon,
		Semicolon,
		DotDotDot,
		Arrow,
		FatArrow,

		// comments (these will never make their way to the parser!)
		CommentStart,
		CommentMultilineStart,
	};

	enum class Keyword {
		Import,
		Module,
		Const,
		Mut,
		Func,
		Return,
	};

	enum class Kind {
		Identifier    = 0,
		NumberLiteral = 1,
		StringLiteral = 2,
		CharLiteral   = 3,
		Symbol        = 4,
	};

	typedef std::variant<std::string_view, std::string_view, std::string_view, char, Symbol> value_t;

	size_t  begin;
	value_t value;

	size_t size() const;

	inline size_t end() const { return begin + size(); }

	explicit Token(size_t begin, decltype(Token::value)&& value) : begin(begin), value(std::move(value)) {}

	inline static Token make_identifier(size_t begin, std::string_view identifier) {
		return Token(begin, value_t {std::in_place_index<(size_t) Kind::Identifier>, identifier});
	}

	inline static Token make_number_literal(size_t begin, std::string_view literal) {
		return Token(begin, value_t {std::in_place_index<(size_t) Kind::NumberLiteral>, literal});
	}

	inline static Token make_string_literal(size_t begin, std::string_view literal) {
		return Token(begin, value_t {std::in_place_index<(size_t) Kind::StringLiteral>, literal});
	}

	inline static Token make_char_literal(size_t begin, char value) {
		return Token(begin, value_t {std::in_place_index<(size_t) Kind::CharLiteral>, value});
	}

	inline static Token make_symbol(size_t begin, Symbol symbol) {
		return Token(begin, value_t {std::in_place_index<(size_t) Kind::Symbol>, symbol});
	}
};

std::ostream& operator<<(std::ostream&, Token const&);
std::ostream& operator<<(std::ostream&, Token::Symbol const&);
