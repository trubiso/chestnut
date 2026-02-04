#pragma once
#include "diagnostic.hpp"
#include "stream.hpp"
#include "token.hpp"

#include <optional>
#include <string_view>
#include <vector>

class Lexer {
public:
	explicit Lexer(FileContext& context, std::string_view source)
		: context(context)
		, stream_(std::move(source))
		, tokens_() {}

	bool advance();

	inline std::vector<Token>& collect_all() {
		while (advance()) {}
		return tokens();
	}

	inline Stream<char>& stream() { return stream_; }

	inline std::vector<Token>& tokens() { return tokens_; }

	inline std::vector<Diagnostic>& diagnostics() { return diagnostics_; }

	FileContext& context;

	static std::optional<char> lookup_escaped(char x);

private:
	static inline bool is_whitespace(char x) { return x == ' ' || x == '\t' || x == '\r'; }

	static inline bool is_digit(char x) { return '0' <= x && x <= '9'; }

	static inline bool is_binary_digit(char x) { return x == '0' || x == '1'; }

	static inline bool is_octal_digit(char x) { return '0' <= x && x <= '7'; }

	static inline bool is_hex_digit(char x) {
		return ('0' <= x && x <= '9') || ('A' <= x && x <= 'F') || ('a' <= x && x <= 'f');
	}

	static inline bool is_alpha(char x) { return ('A' <= x && x <= 'Z') || ('a' <= x && x <= 'z'); }

	static inline bool is_id_start(char x) { return is_alpha(x) || x == '_'; }

	static inline bool is_id_continue(char x) { return is_alpha(x) || is_digit(x) || x == '_'; }

	static inline std::optional<Token::Symbol> get_symbol_start(char x) {
		switch (x) {
		case '+': return Token::Symbol::Plus;
		case '-': return Token::Symbol::Minus;
		case '*': return Token::Symbol::Star;
		case '/': return Token::Symbol::Div;
		case '&': return Token::Symbol::Amp;
		case '|': return Token::Symbol::Bar;
		case '^': return Token::Symbol::Xor;
		case '~': return Token::Symbol::Tilde;
		case '=': return Token::Symbol::Eq;
		case '<': return Token::Symbol::Lt;
		case '>': return Token::Symbol::Gt;
		case '!': return Token::Symbol::Bang;
		case '?': return Token::Symbol::Maybe;
		case '%': return Token::Symbol::Percent;
		case '@': return Token::Symbol::At;
		case '.': return Token::Symbol::Dot;
		case '(': return Token::Symbol::LParen;
		case ')': return Token::Symbol::RParen;
		case '[': return Token::Symbol::LBracket;
		case ']': return Token::Symbol::RBracket;
		case '{': return Token::Symbol::LBrace;
		case '}': return Token::Symbol::RBrace;
		case ',': return Token::Symbol::Comma;
		case ':': return Token::Symbol::Colon;
		case ';': return Token::Symbol::Semicolon;
		}
		return {};
	}

	static inline bool is_symbol_start(char x) { return get_symbol_start(x).has_value(); }

	inline void consume_whitespace() { stream_.consume_while(is_whitespace); }

	inline void consume_identifier() { stream_.consume_while(is_id_continue); }

	void consume_number_literal();
	void consume_string_literal();
	void consume_char_literal();
	/// If a comment is encountered, returns one of the special comment symbols.
	Token::Symbol consume_symbol();

	Stream<char>            stream_;
	std::vector<Token>      tokens_;
	std::vector<Diagnostic> diagnostics_;
};
