#include "parser.hpp"

#include "lexer.hpp"

#include <cassert>
#include <format>
#include <iostream>
#include <string_view>

namespace AST {

#define SPANNED(fn) spanned((std::function<decltype(fn())()>) [this] { return fn(); })
#define SPANNED_REASON(fn, reason)                                                                               \
	spanned((std::function<decltype(fn(std::declval<decltype(reason)>()))()>) [this] { return fn(reason); })

bool Parser::consume_keyword(Keyword keyword) {
	if (!peek_keyword(keyword)) return false;
	tokens_.advance();
	return true;
}

bool Parser::consume_symbol(Token::Symbol symbol) {
	if (!peek_symbol(symbol)) return false;
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

std::optional<std::string_view> Parser::consume_tag() {
	if (!consume_symbol(Token::Symbol::At)) return {};
	std::optional<std::string_view> name = expect_identifier("expected an identifier (`@` starts a tag)");
	if (!name.has_value()) return {};
	return name;
}

std::optional<QualifiedIdentifier> Parser::consume_qualified_identifier() {
	// NOTE: in the future, we will have to account for static members (T::a) and potentially discarded identifiers
	// at the beginning (_::a)

	// if a :: can be consumed before the qualified identifier, it will be consumed and will turn the qualified
	// identifier absolute
	bool absolute = consume_symbol(Token::Symbol::ColonColon);
	// if :: was consumed, that means this is definitely and unambiguously a qualified identifier now
	std::optional<Spanned<std::string_view>> root
		= absolute ? SPANNED_REASON(
				     expect_identifier,
				     "expected an identifier (`::` starts an absolute qualified identifier)"
			     )
	                   : SPANNED(consume_identifier);
	// now if we didn't find the root we can safely return because, if :: has been parsed, we'll have skipped it :-)
	if (!root.has_value()) return {};
	std::vector<Spanned<std::string_view>> path {root.value()};
	while (consume_symbol(Token::Symbol::ColonColon)) {
		std::optional<Spanned<std::string_view>> piece = SPANNED_REASON(
			expect_identifier,
			"expected an identifier (there is a trailing `::` in a preceding qualified identifier)"
		);
		if (!piece.has_value()) return QualifiedIdentifier {absolute, path};
		path.push_back(piece.value());
	}
	return QualifiedIdentifier {absolute, path};
}

bool Parser::peek_symbol(Token::Symbol symbol) const {
	auto maybe_token = tokens_.peek();
	if (!maybe_token.has_value()) return false;
	Token token = maybe_token.value();
	if (!token.is_symbol()) return false;
	return token.get_symbol() == symbol;
}

bool Parser::peek_keyword(Keyword keyword) const {
	auto maybe_token = tokens_.peek();
	if (!maybe_token.has_value()) return false;
	Token token = maybe_token.value();
	if (!token.is_identifier()) return false;
	switch (keyword) {
	case Keyword::Import: return token.get_identifier() == "import";
	case Keyword::Module: return token.get_identifier() == "module";
	case Keyword::Const:  return token.get_identifier() == "const";
	case Keyword::Mut:    return token.get_identifier() == "mut";
	case Keyword::Func:   return token.get_identifier() == "func";
	case Keyword::Return: return token.get_identifier() == "return";
	}
	[[assume(false)]];
	return false;
}

// TODO: merge expected diagnoses

bool Parser::expect_symbol(std::string_view reason, Token::Symbol symbol) {
	if (consume_symbol(symbol)) return true;
	Token last_token = tokens_.peek().value_or(tokens_.last());

	std::string title = std::format("expected symbol '{}'", get_variant_name(symbol));

	diagnostics_.push_back(Diagnostic::error(
		std::move(title),
		std::string(reason),
		{Diagnostic::Sample(last_token.span())}
	));
	return false;
}

std::optional<std::string_view> Parser::expect_identifier(std::string_view reason) {
	auto identifier = consume_identifier();
	if (identifier.has_value()) return identifier;
	Token last_token = tokens_.peek().value_or(tokens_.last());
	diagnostics_.push_back(Diagnostic::error(
		"expected identifier",
		std::string(reason),
		{Diagnostic::Sample(last_token.span())}
	));
	return {};
}

void Parser::skip_semis() {
	tokens_.consume_while([](Token token) {
		return token.is_symbol() && token.get_symbol() == Token::Symbol::Semicolon;
	});
}

std::optional<Module> Parser::parse_module() {
	if (!consume_keyword(Keyword::Module)) return {};
	// TODO: expect everything from here on out
	std::optional<Spanned<std::string_view>> name = SPANNED(consume_identifier);
	if (!name.has_value()) return {};
	std::optional<Module::Body> body = parse_module_body();
	if (!body.has_value()) return {};
	std::cout
		<< "found module with name "
		<< name.value().value
		<< " and "
		<< body.value().items.size()
		<< " items"
		<< std::endl;
	return Module {name, body.value()};
}

std::optional<Module::Item> Parser::parse_module_item() {
	// TODO: visibility qualifiers
	std::optional<Module::Item> item;
	if (peek_keyword(Keyword::Module)) {
		item = parse_module();
	} else if (peek_keyword(Keyword::Func)) {
		item = parse_function();
	}
	if (item.has_value()) return item.value();
	else return {};
}

std::optional<Module::Body> Parser::parse_module_body(bool bare) {
	if (!bare)
		if (!expect_symbol("expected opening brace to begin module body", Token::Symbol::LBrace)) return {};
	skip_semis();
	std::vector<Module::Item> items {};
	for (auto item = parse_module_item(); item.has_value(); item = parse_module_item()) {
		items.push_back(item.value());
		skip_semis();
	}
	skip_semis();
	if (!bare) expect_symbol("expected closing brace to end module body", Token::Symbol::RBrace);
	return Module::Body {items};
}

std::optional<Function> Parser::parse_function() {
	if (!consume_keyword(Keyword::Func)) return {};
	auto name = SPANNED_REASON(expect_identifier, "expected function name");
	if (!name.has_value()) return {};
	if (!expect_symbol("expected opening parenthesis to begin argument list", Token::Symbol::LParen)) return {};
	// parse args
	std::vector<Function::Argument> arguments {};
	while (true) {
		auto argument_name = SPANNED(consume_identifier);
		if (!argument_name.has_value()) break;
		if (!expect_symbol("expected ':' to specify argument type", Token::Symbol::Colon)) return {};
		auto argument_type = SPANNED_REASON(expect_identifier, "expected argument type");
		if (!argument_type.has_value()) return {};
		arguments.emplace_back(argument_name.value(), argument_type.value());
		while (peek_symbol(Token::Symbol::Comma)) tokens_.advance();
	}
	if (!expect_symbol("expected closing parenthesis to end argument list", Token::Symbol::RParen)) return {};
	auto return_type = SPANNED(consume_identifier);  // TODO: type
	std::cout << "found function with name " << name.value().value << ", " << arguments.size() << " arguments: ";
	for (Function::Argument const& argument : arguments) {
		std::cout << argument.name.value << " of type " << argument.type.value << ", ";
	}
	std::cout << std::endl;
	return Function {name.value(), arguments, return_type};
}

bool Parser::advance() {
	if (!tokens_.has_value()) return false;  // EOF
	skip_semis();
	return parse_module().has_value();
}

}  // namespace AST
