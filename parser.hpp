#pragma once
#include "ast/expression.hpp"
#include "ast/qualified_identifier.hpp"
#include "ast/statement.hpp"
#include "ast/type.hpp"
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

struct Function {
	struct Argument {
		Spanned<std::string_view> name;
		Spanned<Type>             type;
	};

	Spanned<std::string_view>    name;
	std::vector<Argument>        arguments;
	std::optional<Spanned<Type>> return_type;

	std::optional<Scope> body;
};

struct Module {
	std::optional<Spanned<std::string_view>> name;

	// we cannot make this a struct, because C++ does not allow incomplete types in variants (which is fair, but we
	// know this will be on the heap anyways).
	// the boolean means whether this item is exported or not.
	using Item = std::tuple<bool, std::variant<Function, Module>>;

	struct Body {
		std::vector<Spanned<Item>> items;
	} body;
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
		Export,
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

	std::optional<Expression>                         consume_expression_atom();
	std::optional<Expression::FunctionCall::Argument> consume_expression_function_call_argument();
	std::optional<Expression>                         consume_expression_function_call();
	std::optional<Expression>                         consume_expression_unary_l1();
	std::optional<Expression>                         consume_expression_binop_l1();
	std::optional<Expression>                         consume_expression();

	std::optional<Statement> consume_statement_declare();
	std::optional<Statement> consume_statement_set();
	std::optional<Statement> consume_statement_expression(Expression&&);
	std::optional<Statement> consume_statement_return();
	std::optional<Statement> consume_statement_scope();
	std::optional<Statement> consume_statement();

	std::optional<Scope> consume_scope();

	// peek_ methods do not increment the index.
	bool peek_symbol(Token::Symbol) const;
	bool peek_keyword(Keyword) const;

	// expect_ methods do the same as consume_, but throw a diagnostic as well
	// upon failure. The reason string is only copied if a diagnostic is thrown.
	bool expect_symbol(std::string_view reason, Token::Symbol);

	inline bool expect_semicolon(std::string_view reason) {
		return expect_symbol(reason, Token::Symbol::Semicolon);
	}

	std::optional<std::string_view> expect_identifier(std::string_view reason);

	std::optional<Type> expect_type(std::string_view reason);

	std::optional<Expression> expect_expression_atom(std::string_view reason);
	std::optional<Expression> expect_expression_function_call(std::string_view reason);
	std::optional<Expression> expect_expression_unary_l1(std::string_view reason);
	std::optional<Expression> expect_expression_binop_l1(std::string_view reason);
	std::optional<Expression> expect_expression(std::string_view reason);

	std::optional<Scope> expect_scope(std::string_view reason);

	// skip semicolons
	void skip_semis();

	std::optional<Module>       parse_module();
	std::optional<Module::Item> parse_module_item();
	std::optional<Module::Body> parse_module_body(bool bare = false);

	std::optional<Function> parse_function();
};

}  // namespace AST
