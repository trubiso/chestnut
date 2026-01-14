#include "parser.hpp"

#include "lexer.hpp"

#include <cassert>
#include <charconv>
#include <format>
#include <iostream>
#include <string>
#include <string_view>

namespace AST {

std::ostream& operator<<(std::ostream& os, QualifiedIdentifier const& identifier) {
	if (identifier.absolute) os << "::";
	for (size_t i = 0; i < identifier.path.size(); ++i) {
		os << identifier.path[i].value;
		if (i + 1 < identifier.path.size()) os << "::";
	}
	return os;
}

std::ostream& operator<<(std::ostream& os, Expression::Atom::NumberLiteral const& literal) {
	if (literal.suffix.has_value()) {
		return os << '[' << literal.literal << " w/ suffix " << literal.suffix.value() << ']';
	} else {
		return os << literal.literal;
	}
}

std::ostream& operator<<(std::ostream& os, Expression::Atom::StringLiteral const& literal) {
	if (literal.suffix.has_value()) {
		return os << '[' << literal.literal << " w/ suffix " << literal.suffix.value() << ']';
	} else {
		return os << literal.literal;
	}
}

std::ostream& operator<<(std::ostream& os, Expression::Atom::CharLiteral const& literal) {
	// FIXME: we must escape the char
	if (literal.suffix.has_value()) {
		return os << "['" << literal.literal << "' w/ suffix " << literal.suffix.value() << ']';
	} else {
		return os << '\'' << literal.literal << '\'';
	}
}

std::ostream& operator<<(std::ostream& os, Expression::Atom const& atom) {
	switch ((Expression::Atom::Kind) atom.value.index()) {
	case Expression::Atom::Kind::Identifier:    return os << atom.get_identifier();
	case Expression::Atom::Kind::NumberLiteral: return os << atom.get_number_literal();
	case Expression::Atom::Kind::StringLiteral: return os << atom.get_string_literal();
	case Expression::Atom::Kind::CharLiteral:   return os << atom.get_char_literal();
	case Expression::Atom::Kind::Expression:    return os << '(' << *atom.get_expression() << ')';
	}
}

std::ostream& operator<<(std::ostream& os, Expression::UnaryOperation const& operation) {
	return os << '(' << operation.operation << operation.operand->value << ')';
}

std::ostream& operator<<(std::ostream& os, Expression::BinaryOperation const& operation) {
	return os << '(' << operation.lhs->value << ' ' << operation.operation << ' ' << operation.rhs->value << ')';
}

std::ostream& operator<<(std::ostream& os, Expression const& expression) {
	switch ((Expression::Kind) expression.value.index()) {
	case Expression::Kind::Atom:            return os << expression.get_atom();
	case Expression::Kind::UnaryOperation:  return os << expression.get_unary_operation();
	case Expression::Kind::BinaryOperation: return os << expression.get_binary_operation();
	}
}

std::ostream& operator<<(std::ostream& os, Type const& type) {
	switch ((Type::Kind) type.value.index()) {
	case Type::Kind::Integer: break;
	case Type::Kind::Float:   return os << "float" << (uint32_t) type.get_float().width_value();
	case Type::Kind::Void:    return os << "void";
	case Type::Kind::Char:    return os << "char";
	case Type::Kind::Bool:    return os << "bool";
	}

	// we know it's an integer now
	Type::Integer int_ = type.get_integer();
	os << (int_.is_signed() ? "int" : "uint");
	Type::Integer::WidthType width_type = int_.width_type();

	switch (width_type) {
	case Type::Integer::WidthType::Fixed: return os << int_.bit_width().value();
	case Type::Integer::WidthType::Any:   return os;
	case Type::Integer::WidthType::Ptr:   return os << "ptr";
	case Type::Integer::WidthType::Size:  return os << "size";
	}
}

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

std::optional<std::string_view> Parser::consume_number_literal() {
	auto maybe_token = tokens_.peek();
	if (!maybe_token.has_value()) return {};
	Token token = maybe_token.value();
	if (!token.is_number_literal()) return {};
	tokens_.advance();
	return token.get_number_literal();
}

std::optional<std::string_view> Parser::consume_string_literal() {
	auto maybe_token = tokens_.peek();
	if (!maybe_token.has_value()) return {};
	Token token = maybe_token.value();
	if (!token.is_string_literal()) return {};
	tokens_.advance();
	return token.get_string_literal();
}

std::optional<char> Parser::consume_char_literal() {
	auto maybe_token = tokens_.peek();
	if (!maybe_token.has_value()) return {};
	Token token = maybe_token.value();
	if (!token.is_char_literal()) return {};
	tokens_.advance();
	return token.get_char_literal();
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

std::optional<Type> Parser::consume_type() {
	// in the future, this will require atoms and operators just like expression.
	// for now, we're only doing built-in types, so it's much easier for us!
	// that's also why we're so harsh on retroceding instead of expecting.

	std::optional<std::string_view> maybe_name = consume_identifier();  // all built-ins are just identifiers
	if (!maybe_name.has_value()) return {};
	std::string_view name = maybe_name.value();

	// variables have to declared at the top so c++ won't wail
	bool                         starts_with_u, starts_with_i;
	uint32_t                     width;
	std::from_chars_result       res;
	std::optional<Type::Integer> int_ {};
	size_t                       base_length = 0;

	// "easy" types
	if (name == "void") return Type::make_void();
	if (name == "char") return Type::make_char();
	if (name == "bool") return Type::make_bool();

	// float types (can be bruteforced)
	if (name.starts_with("float")) {
		// TODO: support arbitrary sized floats (should we?)
		if (name == "float16") return Type::make_float(Type::Float::Width::F16);
		if (name == "float32") return Type::make_float(Type::Float::Width::F32);
		if (name == "float64") return Type::make_float(Type::Float::Width::F64);
		if (name == "float128") return Type::make_float(Type::Float::Width::F128);
		goto none_match;
	}

	// just the integer types left
	starts_with_u = name.starts_with("uint");
	starts_with_i = name.starts_with("int");
	if (!starts_with_u && !starts_with_i) goto none_match;
	// get rid of the easy cases
	base_length = starts_with_u ? 4 : 3;
	if (name.length() == base_length) return Type::make_integer(Type::Integer::any(starts_with_i));
	if (name.length() == base_length + 3 && name.ends_with("ptr"))
		return Type::make_integer(Type::Integer::ptr(starts_with_i));
	if (name.length() == base_length + 4 && name.ends_with("size"))
		return Type::make_integer(Type::Integer::size(starts_with_i));
	// we know the name is at least base_length long, so the following is valid
	res = std::from_chars(name.data() + base_length, name.data() + name.size(), width);
	if (res.ec != std::errc() || res.ptr != name.data() + name.size()) goto none_match;
	int_ = Type::Integer::with_width(width, starts_with_i);
	if (!int_.has_value()) goto none_match;
	return Type::make_integer(std::move(int_.value()));

none_match:
	// we need to un-consume the identifier
	tokens_.retreat();
	return {};
}

std::optional<Expression> Parser::consume_expression_atom() {
	// it could be a potentially qualified identifier
	std::optional<QualifiedIdentifier> identifier = consume_qualified_identifier();
	if (identifier.has_value())
		return Expression::make_atom(Expression::Atom::make_identifier(std::move(identifier.value())));

	// since it's not an identifier, it has to be a literal
	std::optional<std::string_view> sv;
	std::optional<std::string_view> suffix;  // we always try to consume an extra suffix

	if (sv = consume_number_literal(), sv.has_value()) {
		suffix = consume_identifier();
		return Expression::make_atom(Expression::Atom::make_number_literal(sv.value(), suffix));
	}

	if (sv = consume_string_literal(), sv.has_value()) {
		suffix = consume_identifier();
		return Expression::make_atom(Expression::Atom::make_string_literal(sv.value(), suffix));
	}

	std::optional<char> c;

	if (c = consume_char_literal(), c.has_value()) {
		suffix = consume_identifier();
		return Expression::make_atom(Expression::Atom::make_char_literal(c.value(), suffix));
	}

	if (consume_symbol(Token::Symbol::LParen)) {
		// TODO: review error handling here
		std::optional<Expression> expression = expect_expression("expected expression inside parentheses");
		if (!expression.has_value()) return {};
		if (!expect_symbol(
			    "expected closing parenthesis to end parenthesized expression",
			    Token::Symbol::RParen
		    ))
			return expression;
		return expression;
	}

	return {};
}

// TODO: greatly improve this code with macros or some system that doesn't require this much repetition

std::optional<Expression> Parser::consume_expression_unary_l1() {
	// this refers to unary negation. this code is really ugly
	size_t negations = 0;
	while (consume_symbol(Token::Symbol::Minus)) negations++;
	std::optional<Spanned<Expression>> should_operand
		= negations ? SPANNED_REASON(expect_expression_atom, "expected expression after unary operator")
	                    : SPANNED(consume_expression_atom);
	if (!should_operand.has_value()) return {};
	Spanned<Expression> operand = std::move(should_operand.value());
	while (negations--)
		operand = Spanned<Expression> {
			Span(operand.span.start - 1, operand.span.end),
			Expression::make_unary_operation(
				std::make_unique<Spanned<Expression>>(std::move(operand)),
				Token::Symbol::Minus
			)
		};
	return std::move(operand.value);
}

std::optional<Expression> Parser::consume_expression_binop_l1() {
	// highest precedence will be mul/div
	std::optional<Spanned<Expression>> maybe_lhs = SPANNED(consume_expression_unary_l1);
	if (!maybe_lhs.has_value()) return {};
	Spanned<Expression> lhs = std::move(maybe_lhs.value());

	bool has_star = false, has_div = false;
	while ((has_star = peek_symbol(Token::Symbol::Star)) || (has_div = peek_symbol(Token::Symbol::Div))) {
		// consume whichever of the symbols it was
		Token::Symbol operation = has_star ? Token::Symbol::Star : Token::Symbol::Div;
		tokens_.advance();

		std::optional<Spanned<Expression>> should_rhs
			= SPANNED_REASON(expect_expression_unary_l1, "expected expression after binary operator");
		if (!should_rhs.has_value()) return std::move(lhs.value);  // error recovery
		Spanned<Expression> rhs = std::move(should_rhs.value());

		lhs = Spanned<Expression> {
			Span(lhs.span.start, rhs.span.end),
			Expression::make_binary_operation(
				std::make_unique<Spanned<Expression>>(std::move(lhs)),
				std::make_unique<Spanned<Expression>>(std::move(rhs)),
				operation
			)
		};
	}

	return std::move(lhs.value);
}

std::optional<Expression> Parser::consume_expression() {
	// highest precedence will be mul/div
	std::optional<Spanned<Expression>> maybe_lhs = SPANNED(consume_expression_binop_l1);
	if (!maybe_lhs.has_value()) return {};
	Spanned<Expression> lhs = std::move(maybe_lhs.value());

	bool has_plus = false, has_minus = false;
	while ((has_plus = peek_symbol(Token::Symbol::Plus)) || (has_minus = peek_symbol(Token::Symbol::Minus))) {
		// consume whichever of the symbols it was
		Token::Symbol operation = has_plus ? Token::Symbol::Plus : Token::Symbol::Minus;
		tokens_.advance();

		std::optional<Spanned<Expression>> should_rhs
			= SPANNED_REASON(expect_expression_binop_l1, "expected expression after binary operator");
		if (!should_rhs.has_value()) return std::move(lhs.value);  // error recovery
		Spanned<Expression> rhs = std::move(should_rhs.value());

		lhs = Spanned<Expression> {
			Span(lhs.span.start, rhs.span.end),
			Expression::make_binary_operation(
				std::make_unique<Spanned<Expression>>(std::move(lhs)),
				std::make_unique<Spanned<Expression>>(std::move(rhs)),
				operation
			)
		};
	}

	return std::move(lhs.value);
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

// TODO: reduce boilerplate in these methods

bool Parser::expect_symbol(std::string_view reason, Token::Symbol symbol) {
	if (consume_symbol(symbol)) return true;
	Token last_token = tokens_.peek().value_or(tokens_.last());

	std::string title = std::format("expected symbol '{}'", get_variant_name(symbol));

	diagnostics_.push_back(
		Diagnostic::error(std::move(title), std::string(reason), {Diagnostic::Sample(last_token.span())})
	);
	return false;
}

std::optional<std::string_view> Parser::expect_identifier(std::string_view reason) {
	auto identifier = consume_identifier();
	if (identifier.has_value()) return identifier;
	Token last_token = tokens_.peek().value_or(tokens_.last());
	diagnostics_.push_back(
		Diagnostic::error("expected identifier", std::string(reason), {Diagnostic::Sample(last_token.span())})
	);
	return {};
}

std::optional<Type> Parser::expect_type(std::string_view reason) {
	auto type = consume_type();
	if (type.has_value()) return type;
	Token last_token = tokens_.peek().value_or(tokens_.last());
	diagnostics_.push_back(
		Diagnostic::error("expected type", std::string(reason), {Diagnostic::Sample(last_token.span())})
	);
	return {};
}

std::optional<Expression> Parser::expect_expression_atom(std::string_view reason) {
	auto expression = consume_expression_atom();
	if (expression.has_value()) return expression;
	Token last_token = tokens_.peek().value_or(tokens_.last());
	diagnostics_.push_back(
		Diagnostic::error("expected expression", std::string(reason), {Diagnostic::Sample(last_token.span())})
	);
	return {};
}

std::optional<Expression> Parser::expect_expression_unary_l1(std::string_view reason) {
	auto expression = consume_expression_unary_l1();
	if (expression.has_value()) return expression;
	Token last_token = tokens_.peek().value_or(tokens_.last());
	diagnostics_.push_back(
		Diagnostic::error("expected expression", std::string(reason), {Diagnostic::Sample(last_token.span())})
	);
	return {};
}

std::optional<Expression> Parser::expect_expression_binop_l1(std::string_view reason) {
	auto expression = consume_expression_binop_l1();
	if (expression.has_value()) return expression;
	Token last_token = tokens_.peek().value_or(tokens_.last());
	diagnostics_.push_back(
		Diagnostic::error("expected expression", std::string(reason), {Diagnostic::Sample(last_token.span())})
	);
	return {};
}

std::optional<Expression> Parser::expect_expression(std::string_view reason) {
	auto expression = consume_expression();
	if (expression.has_value()) return expression;
	Token last_token = tokens_.peek().value_or(tokens_.last());
	diagnostics_.push_back(
		Diagnostic::error("expected expression", std::string(reason), {Diagnostic::Sample(last_token.span())})
	);
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
		auto argument_type = SPANNED_REASON(expect_type, "expected argument type");
		if (!argument_type.has_value()) return {};
		arguments.emplace_back(argument_name.value(), argument_type.value());
		while (peek_symbol(Token::Symbol::Comma)) tokens_.advance();
	}
	if (!expect_symbol("expected closing parenthesis to end argument list", Token::Symbol::RParen)) return {};
	auto return_type = SPANNED(consume_type);
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
