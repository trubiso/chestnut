#pragma once
#include "ast/expression.hpp"
#include "ast/function.hpp"
#include "ast/identifier.hpp"
#include "ast/module.hpp"
#include "ast/statement.hpp"
#include "ast/tag.hpp"
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

struct ExpectedDiagnostic {
	struct Expectation {
		std::string what;
		std::string why;

		bool operator==(Expectation const&) const = default;
	};

	// TODO: SmallVec this up
	std::vector<Expectation> expectations;

	Span where;

	Diagnostic as_diagnostic(FileContext const& context) const;

	inline bool has_expectation(Expectation const& expectation) {
		return std::find(expectations.cbegin(), expectations.cend(), expectation) != expectations.cend();
	}
};

class Parser {
public:
	explicit Parser(FileContext const& context, Stream<Token>&& tokens) : context_(context), tokens_(tokens) {}

	std::vector<Diagnostic> diagnostics() const;

	Module parse_all(std::string name);

private:
	enum class Keyword {
		Import,
		Module,
		Export,
		Const,
		Mut,
		Anon,
		Func,
		Return,
	};

	FileContext   context_;
	Stream<Token> tokens_;

	std::vector<std::variant<ExpectedDiagnostic, Diagnostic>> diagnostics_;

	template <typename T>
	inline std::optional<Spanned<T>> spanned(std::function<std::optional<T>()> function) {
		if (!tokens_.peek().has_value()) return {};
		size_t begin = tokens_.peek().value().begin;

		std::optional<T> value = function();
		if (!value.has_value()) return {};

		// FIXME: this is wrong
		size_t end = tokens_.peek().value_or(tokens_.last()).begin;

		return Spanned<T> {Span(begin, end), std::move(value.value())};
	}

	// consume_ methods return a non-null/true value if they found a token
	// abiding by the specified criteria and increment the index
	bool consume_keyword(Keyword);
	bool consume_symbol(Token::Symbol);

	bool consume_single_comma_or_more();

	std::optional<std::string> consume_number_literal();
	std::optional<std::string> consume_string_literal();
	std::optional<std::string> consume_char_literal();

	std::optional<std::string> consume_bare_unqualified_identifier();
	std::optional<Identifier>  consume_unqualified_identifier();
	std::optional<Identifier>  consume_identifier();

	std::optional<Tag> consume_tag();

	std::optional<Type::Atom> consume_type_atom();
	std::optional<Type>       consume_type();

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
	bool peek_unqualified_identifier() const;

	// expect_ methods do the same as consume_, but throw a diagnostic as well
	// upon failure. The reason string is only copied if a diagnostic is thrown.
	void add_expected_diagnostic(std::string_view what, std::string_view why);

	bool expect_symbol(std::string_view reason, Token::Symbol);

	// TODO: point one token further back so it is clearer where the semicolon is missing
	inline bool expect_semicolon(std::string_view reason) {
		return expect_symbol(reason, Token::Symbol::Semicolon);
	}

	std::optional<std::string> expect_bare_unqualified_identifier(std::string_view reason);
	std::optional<Identifier>  expect_unqualified_identifier(std::string_view reason);
	std::optional<Identifier>  expect_identifier(std::string_view reason);

	std::optional<Type> expect_type(std::string_view reason);

	std::optional<Expression> expect_expression_atom(std::string_view reason);
	std::optional<Expression> expect_expression_function_call(std::string_view reason);
	std::optional<Expression> expect_expression_unary_l1(std::string_view reason);
	std::optional<Expression> expect_expression_binop_l1(std::string_view reason);
	std::optional<Expression> expect_expression(std::string_view reason);

	std::optional<Scope> expect_scope(std::string_view reason);

	// skip semicolons
	void skip_semis();

	std::optional<Function>     parse_function();
	std::optional<Import>       parse_import();
	std::optional<Module>       parse_module();
	std::optional<Module::Item> parse_module_item();
	std::optional<Module::Body> parse_module_body(bool bare = false);
};

}  // namespace AST
