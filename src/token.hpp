#pragma once
#include "span.hpp"

#include <cstddef>
#include <ostream>
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
		AmpAmp,
		Bar,
		BarBar,
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

	// TODO: make special case for the _ identifier which is treated as discard/nothing.

	enum class Kind { Identifier, Label, NumberLiteral, StringLiteral, CharLiteral, Symbol };

	typedef std::variant<std::string, std::string, std::string, std::string, std::string, Symbol> value_t;

	size_t  begin;
	value_t value;

	inline constexpr Kind kind() const { return (Kind) value.index(); }

	size_t size() const;

	inline size_t end() const { return begin + size(); }

	explicit Token(size_t begin, decltype(Token::value)&& value) : begin(begin), value(std::move(value)) {}

	inline static Token make_identifier(size_t begin, std::string identifier) {
		return Token(begin, value_t {std::in_place_index<(size_t) Kind::Identifier>, identifier});
	}

	inline static Token make_label(size_t begin, std::string label) {
		return Token(begin, value_t {std::in_place_index<(size_t) Kind::Label>, label});
	}

	inline static Token make_number_literal(size_t begin, std::string literal) {
		return Token(begin, value_t {std::in_place_index<(size_t) Kind::NumberLiteral>, literal});
	}

	inline static Token make_string_literal(size_t begin, std::string literal) {
		return Token(begin, value_t {std::in_place_index<(size_t) Kind::StringLiteral>, literal});
	}

	inline static Token make_char_literal(size_t begin, std::string value) {
		return Token(begin, value_t {std::in_place_index<(size_t) Kind::CharLiteral>, value});
	}

	inline static Token make_symbol(size_t begin, Symbol symbol) {
		return Token(begin, value_t {std::in_place_index<(size_t) Kind::Symbol>, symbol});
	}

	inline bool is_identifier() const { return kind() == Kind::Identifier; }

	inline bool is_label() const { return kind() == Kind::Label; }

	inline bool is_number_literal() const { return kind() == Kind::NumberLiteral; }

	inline bool is_string_literal() const { return kind() == Kind::StringLiteral; }

	inline bool is_char_literal() const { return kind() == Kind::CharLiteral; }

	inline bool is_symbol() const { return kind() == Kind::Symbol; }

	inline std::string const& get_identifier() const { return std::get<(size_t) Kind::Identifier>(value); }

	inline std::string const& get_label() const { return std::get<(size_t) Kind::Label>(value); }

	inline std::string const& get_number_literal() const { return std::get<(size_t) Kind::NumberLiteral>(value); }

	inline std::string const& get_string_literal() const { return std::get<(size_t) Kind::StringLiteral>(value); }

	inline std::string const& get_char_literal() const { return std::get<(size_t) Kind::CharLiteral>(value); }

	inline Symbol get_symbol() const { return std::get<(size_t) Kind::Symbol>(value); }

	inline Span span() const { return Span(begin, end()); }
};

std::ostream& operator<<(std::ostream&, Token const&);
std::ostream& operator<<(std::ostream&, Token::Symbol const&);

char const* get_variant_name(Token::Symbol);
