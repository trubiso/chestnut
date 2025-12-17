#include "lexer.hpp"
#include "span.hpp"
#include "stream.hpp"
#include "token.hpp"

#include <functional>
#include <optional>
#include <string_view>
#include <variant>
#include <vector>

namespace AST {

// TODO: create a StringInterner to avoid moving around string views constantly

// TODO: type
typedef std::string_view Type;

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
	Stream<Token>           tokens_;
	std::vector<Diagnostic> diagnostics_;

	template <typename T>
	inline std::optional<Spanned<T>> spanned(std::function<std::optional<T>()> function) {
		if (!tokens_.peek().has_value()) return {};
		size_t begin = tokens_.peek().value().begin;

		std::optional<T> value = function();
		if (!value.has_value()) return {};

		size_t end = tokens_.peek().value_or(tokens_.last()).begin;

		return Spanned<T> {Span(begin, end), value.value()};
	}

	// TODO: introduce qualified identifiers

	// consume_ methods return a non-null/false value if they found a token
	// abiding by the specified criteria and increment the index
	bool                            consume_identifier(std::string_view);
	std::optional<std::string_view> consume_identifier();
	bool                            consume_symbol(Token::Symbol);

	// consume_spanned_ methods do the same, but return a spanned value.
	std::optional<Spanned<std::string_view>> consume_spanned_identifier();

	// peek_ methods do not increment the index.
	bool peek_symbol(Token::Symbol);
	bool peek_identifier(std::string_view);

	// expect_ methods do the same as consume_, but throw a diagnostic as well
	// upon failure. The reason string is only copied if a diagnostic is thrown.
	bool                            expect_symbol(std::string_view reason, Token::Symbol);
	std::optional<std::string_view> expect_identifier(std::string_view reason);

	// skip semicolons
	void skip_semis();

	std::optional<Module>       parse_module();
	std::optional<Module::Item> parse_module_item();
	std::optional<Module::Body> parse_module_body(bool bare = false);

	std::optional<Function> parse_function();

	// TODO: parse_item which parses a module item (submmodule, function, in the future also structs and global
	// vars)
};

}  // namespace AST
