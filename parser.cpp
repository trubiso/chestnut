#include "parser.hpp"

#include "lexer.hpp"

#include <cassert>
#include <format>
#include <iostream>

namespace AST {

bool Parser::consume_identifier(std::string_view name) {
	auto maybe_token = tokens_.peek();
	if (!maybe_token.has_value()) return false;
	Token token = maybe_token.value();
	if (!token.is_identifier()) return false;
	if (token.get_identifier() != name) return false;
	tokens_.advance();
	return true;
}

std::optional<std::string_view> Parser::consume_identifier() {
	auto maybe_token = tokens_.peek();
	if (!maybe_token.has_value()) return {};
	Token token = maybe_token.value();
	if (!token.is_identifier()) return {};
	tokens_.advance();
	return token.get_identifier();
}

bool Parser::consume_symbol(Token::Symbol symbol) {
	if (!peek_symbol(symbol)) return false;
	tokens_.advance();
	return true;
}

bool Parser::peek_symbol(Token::Symbol symbol) {
	auto maybe_token = tokens_.peek();
	if (!maybe_token.has_value()) return false;
	Token token = maybe_token.value();
	if (!token.is_symbol()) return false;
	return token.get_symbol() == symbol;
}

bool Parser::expect_symbol(Token::Symbol symbol) {
	if (consume_symbol(symbol)) return true;
	// TODO: add symbol to diagnostic
	Token last_token = tokens_.peek().value_or(tokens_.last());
	diagnostics_.push_back(Diagnostic(
		Diagnostic::Severity::Error,
		"expected symbol",
		std::format("expected '{}'", get_variant_name(symbol)),
		{Diagnostic::Label(last_token.span())}
	));
	return false;
}

void Parser::skip_semis() {
	tokens_.consume_while([](Token token) {
		return token.is_symbol() && token.get_symbol() == Token::Symbol::Semicolon;
	});
}

#define SPANNED(fn) spanned((std::function<decltype(fn())()>) [this] { return fn(); })

std::optional<Module> Parser::parse_module() {
	if (!consume_identifier("module")) return {};
	std::optional<Spanned<std::string_view>> name = SPANNED(consume_identifier);
	if (!name.has_value()) return {};
	std::optional<Module::Body> body = parse_module_body();
	if (!body.has_value()) return {};
	std::cout
		<< "found module with name "
		<< name.value().value
		<< " and "
		<< body.value().submodules.size()
		<< " submodules"
		<< std::endl;
	return Module {name, body.value()};
}

std::optional<Module::Body> Parser::parse_module_body(bool bare) {
	if (!bare)
		if (!expect_symbol(Token::Symbol::LBrace)) return {};
	skip_semis();
	std::vector<Module> submodules {};
	for (auto submodule = parse_module(); submodule.has_value(); submodule = parse_module()) {
		submodules.push_back(submodule.value());
		skip_semis();
	}
	skip_semis();
	if (!bare) expect_symbol(Token::Symbol::RBrace);
	return Module::Body {submodules};
}

bool Parser::advance() {
	if (!tokens_.has_value()) return false;  // EOF
	skip_semis();
	return parse_module().has_value();
}

}  // namespace AST
