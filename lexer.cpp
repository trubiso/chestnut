#include "lexer.hpp"

#include <cassert>
#include <concepts>
#include <cstdio>
#include <format>
#include <string_view>
#include <utility>

bool Lexer::advance() {
	consume_whitespace();

	auto current = stream_.peek();
	if (!current.has_value()) return false;

	for (current = stream_.peek();
	     current.has_value() && (current.value() == '\n' || is_whitespace(current.value()));
	     stream_.advance(), current = stream_.peek()) {
		if (current.value() == '\n') loc_.push_back(stream_.index() + 1);
	}

	if (!current.has_value()) return false;
	char   current_value = current.value();
	size_t begin         = stream_.index();

	std::optional<Token> token = {};
	Token::Symbol        s;

#define MAKE_STRVIEW_TOKEN(kind)                                            \
	Token(begin,                                                        \
	      Token::value_t {                                              \
		      std::in_place_index<(size_t) kind>,                   \
		      stream_.data().substr(begin, stream_.index() - begin) \
	      })

	if (is_id_start(current_value)) {
		consume_identifier();
		token = MAKE_STRVIEW_TOKEN(Token::Kind::Identifier);
	} else if (is_digit(current_value)) {
		consume_number_literal();
		token = MAKE_STRVIEW_TOKEN(Token::Kind::NumberLiteral);
	} else if (current_value == '"') {
		consume_string_literal();
		token = MAKE_STRVIEW_TOKEN(Token::Kind::StringLiteral);
	} else if (current_value == '\'') {
		token = Token::make_char_literal(begin, consume_char_literal());
	} else if (is_symbol_start(current_value)) {
		s = consume_symbol();
		if (s == Token::Symbol::CommentStart || s == Token::Symbol::CommentMultilineStart) return true;
		token = Token::make_symbol(begin, s);
	} else {
		diagnostics_.push_back(Diagnostic(
			Diagnostic::Severity::Error,
			"unknown/unsupported character",
			std::format("found unknown/unsupported character 0x{:X}", current_value),
			{Diagnostic::Label(Span(stream_.index()))}
		));
		// we have to push a line, otherwise the diagnostics glitch
		for (current = stream_.peek(); current.has_value() && current.value() != '\n';
		     stream_.advance(), current = stream_.peek());
		if (current.has_value()) loc_.push_back(stream_.index() + 1);
		else loc_.push_back(stream_.index());
		// TODO: skip until next parsable character
		return false;
	}

	if (token.has_value()) tokens_.push_back(token.value());

	return true;

#undef MAKE_STRVIEW_TOKEN
}

void consume_base_number_lit_after_zero(Lexer& lexer, std::predicate<char> auto is_base_digit) {
	// try to consume the letter ('b', 'o', 'x')
	if (!lexer.stream().advance()) {
		// if the program ends on 0[b/o/x], [b/o/x] is an identifier
		lexer.stream().retreat();
		return;
	}
	auto current = lexer.stream().peek();
	// it must have a value because stream().advance() is true
	assert(current.has_value());
	if (!is_base_digit(current.value())) {
		// 0[b/o/x] followed by a non-base-digit means that [b/o/x] starts an
		// identifier
		lexer.stream().retreat();
		return;
	}
	// consume any combination of base digits and padding underscores
	lexer.stream().consume_while([is_base_digit](char x) { return is_base_digit(x) || x == '_'; });
}

void Lexer::consume_number_literal() {
	// number literal suffixes are independent identifiers
	// that means that they are handled in the parser
	assert(stream_.peek().has_value());
	char first_number = stream_.peek().value();
	if (!stream_.advance()) return;  // consume first number and gracefully handle one-digit literals
	// check for base
	auto current = stream_.peek();
	assert(current.has_value());
	// if the first number is a 0, there is potential for a literal with base
	if (first_number == '0') switch (current.value()) {
		case 'b':
		case 'B': consume_base_number_lit_after_zero(*this, is_binary_digit); return;

		case 'o':
		case 'O': consume_base_number_lit_after_zero(*this, is_octal_digit); return;

		case 'x':
		case 'X': consume_base_number_lit_after_zero(*this, is_hex_digit); return;

		default: break;
		}
	// this must be a decimal integer or floating point literal
	// consume all decimal digits and padding underscores
	stream_.consume_while([](char x) { return is_digit(x) || x == '_'; });
	current = stream_.peek();
	if (!current.has_value()) return;  // program ends with a number literal
	// current value cannot be a digit, otherwise it would have been detected
	if (current.value() == '.') {  // TODO: more flexible floating point literals (with exps and such)
		// floating point literal
		// we require at least one digit after the dot, otherwise this is a dot
		// access
		if (!stream_.advance()) {
			// the AST is going to be messed up but, for consistency, detect . as
			// symbol
			stream_.retreat();
			return;
		}
		current = stream_.peek();
		assert(current.has_value());
		if (!is_digit(current.value())) {
			// if it's not a digit, dot access
			stream_.retreat();
			return;
		}
		// otherwise, parse all numbers
		stream_.consume_while([](char x) { return is_digit(x) || x == '_'; });
	}
}

std::optional<char> Lexer::lookup_escaped(char x) {
	switch (x) {
	case '0':  return '\0';
	case '\'': return '\'';
	case '"':  return '"';
	case '\\': return '\\';
	case 'a':  return '\a';
	case 'b':  return '\b';
	case 'f':  return '\f';
	case 'n':  return '\n';
	case 'r':  return '\r';
	case 't':  return '\t';
	case 'v':  return '\v';
	}
	return {};
}

void Lexer::consume_string_literal() {
	size_t string_begin = stream_.index();
	stream_.advance();  // consume opening quote
	bool                     found_closing_quote = false;
	decltype(stream_.peek()) current;
	bool                     escape = false;
	while (!found_closing_quote && (current = stream_.consume()).has_value()) {
		if (!escape) switch (current.value()) {
			case '\\': escape = true; break;
			case '"':  found_closing_quote = true; break;
			case '\n': loc_.push_back(stream_.index() + 1);
			default:   break;
			}
		else escape = false;
	}
	if (!found_closing_quote)
		// we only push the beginning quote because otherwise the diagnostic would
		// be huge
		diagnostics_.push_back(Diagnostic(
			Diagnostic::Severity::Error,
			"unterminated string literal",
			{},
			{Diagnostic::Label(Span(string_begin))}
		));
}

char Lexer::consume_char_literal() {
	size_t char_begin = stream_.index();
	stream_.advance();  // consume opening quote
	char                c = '\0';
	std::optional<char> e;
	auto                current = stream_.consume();  // consume the character within
	if (!current.has_value()) goto unclosed_literal;
	switch (current.value()) {
	case '\'':
		// we got '', which is an invalid literal.
		// the closing quote is already consumed.
		diagnostics_.push_back(Diagnostic(
			Diagnostic::Severity::Error,
			"empty character literal",
			{},
			{Diagnostic::Label(Span(stream_.index() - 2, stream_.index()))}
		));
		goto ret;
	case '\\':
		current = stream_.consume();  // escaped character
		if (!current.has_value()) goto unclosed_literal;
		// TODO: support \xFF sequences
		e = lookup_escaped(current.value());
		if (e.has_value()) {
			c = e.value();
		} else {
			// the escape sequence does not exist
			// fallback strategy: use the character
			c = current.value();
			diagnostics_.push_back(Diagnostic(
				Diagnostic::Severity::Error,
				"invalid escape sequence",
				std::format("found invalid sequence \\{}", c),
				{Diagnostic::Label(Span(stream_.index() - 2, stream_.index()))}
			));
		}
		break;
	default:
		// regular character
		c = current.value();
	}

	// find closing quote
	current = stream_.peek();  // consume supposed closing quote
	if (!current.has_value()) goto unclosed_literal;
	if (current.value() != '\'') goto unclosed_literal;
	stream_.advance();
ret:
	return c;

unclosed_literal:
	diagnostics_.push_back(Diagnostic(
		Diagnostic::Severity::Error,
		"unclosed character literal",
		{},
		{Diagnostic::Label(Span(char_begin, stream_.index()))}
	));
	goto ret;
}

Token::Symbol Lexer::consume_symbol() {
	using Symbol = Token::Symbol;
	// transform if the character matches and consume said character, otherwise
	// treat the second character as a separate operator
#define TRANSFORM_IF(c, into)          \
	if (second_character == c) {   \
		partial_symbol = into; \
		stream_.advance();     \
	} else {                       \
		return partial_symbol; \
	}                              \
	break
	// version for 2 cases
#define TRANSFORM_IF2(c1, into1, c2, into2)  \
	if (second_character == c1) {        \
		partial_symbol = into1;      \
		stream_.advance();           \
	} else if (second_character == c2) { \
		partial_symbol = into2;      \
		stream_.advance();           \
	} else {                             \
		return partial_symbol;       \
	}                                    \
	break
	// version for 3 cases
#define TRANSFORM_IF3(c1, into1, c2, into2, c3, into3) \
	if (second_character == c1) {                  \
		partial_symbol = into1;                \
		stream_.advance();                     \
	} else if (second_character == c2) {           \
		partial_symbol = into2;                \
		stream_.advance();                     \
	} else if (second_character == c3) {           \
		partial_symbol = into3;                \
		stream_.advance();                     \
	} else {                                       \
		return partial_symbol;                 \
	}                                              \
	break

	assert(stream_.peek().has_value());
	char first_character = stream_.consume().value();
	assert(is_symbol_start(first_character));
	Symbol partial_symbol = get_symbol_start(first_character).value();

	if (!stream_.peek().has_value()) return partial_symbol;
	char second_character = stream_.peek().value();
	switch (partial_symbol) {
	case Symbol::Eq:    TRANSFORM_IF2('=', Symbol::EqEq, '>', Symbol::FatArrow);
	case Symbol::Lt:    TRANSFORM_IF3('=', Symbol::Le, '>', Symbol::LtGt, '-', Symbol::LeftArrow);
	case Symbol::Gt:    TRANSFORM_IF('=', Symbol::Ge);
	case Symbol::Bang:  TRANSFORM_IF('=', Symbol::Ne);
	case Symbol::Maybe: TRANSFORM_IF2('?', Symbol::MaybeMaybe, '.', Symbol::MaybeDot);
	case Symbol::Dot:   TRANSFORM_IF('.', Symbol::Dot);
	case Symbol::Colon: TRANSFORM_IF(':', Symbol::ColonColon);
	case Symbol::Minus: TRANSFORM_IF('>', Symbol::Arrow);
	case Symbol::Div:   TRANSFORM_IF2('/', Symbol::CommentStart, '*', Symbol::CommentMultilineStart);
	default:
		return partial_symbol;  // if none match, return what we got so far and do
		                        // not consume
	}

	if (partial_symbol == Symbol::CommentStart) {
		stream_.consume_while([](char x) { return x != '\n'; });
		// we are at the newline; it is not consumed so that a semicolon may be
		// inserted
		return Symbol::CommentStart;
	} else if (partial_symbol == Symbol::CommentMultilineStart) {
		size_t comment_begin = stream_.index() - 2;
		size_t nesting       = 1;
		bool   found_slash = false, found_star = false;
		while (stream_.has_value() && nesting > 0) {
			char value = stream_.consume().value();
			if (value == '*') {
				if (found_slash) {
					nesting++;  // found /*
					goto clear_bools;
				} else found_star = true;
			} else if (value == '/') {
				if (found_star) {
					nesting--;  // found */
					goto clear_bools;
				} else found_slash = true;
			} else {
			clear_bools:
				found_star  = false;
				found_slash = false;
			}
			if (value == '\n') loc_.push_back(stream_.index() + 1);
		}
		if (nesting > 0)
			// we only push the beginning because otherwise the diagnostic would be
			// huge
			diagnostics_.push_back(Diagnostic(
				Diagnostic::Severity::Error,
				"unterminated multiline comment",
				{},
				{Diagnostic::Label(Span(comment_begin, comment_begin + 2))}
			));
		return Symbol::CommentMultilineStart;
	}

	// if we got thus far, we have advanced past the second character
	if (!stream_.peek().has_value()) return partial_symbol;
	second_character = stream_.peek().value();
	switch (partial_symbol) {
	case Symbol::DotDot: TRANSFORM_IF3('<', Symbol::DotDotLt, '=', Symbol::DotDotEq, '.', Symbol::DotDotDot);
	default:             return partial_symbol;
	}

	// FIXME: this return is unnecessary, but cppcheck needs it
	[[assume(false)]];
	return partial_symbol;

#undef TRANSFORM_IF
#undef TRANSFORM_IF2
#undef TRANSFORM_IF3
}
