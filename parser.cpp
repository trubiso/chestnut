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
	switch (atom.kind()) {
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

std::ostream& operator<<(std::ostream& os, Expression::FunctionCall const& call) {
	os << "(call " << call.callee->value;
	if (!call.arguments.labeled.empty() || !call.arguments.ordered.empty()) {
		os << " w/ args: (";
		for (size_t i = 0; i < call.arguments.ordered.size(); ++i) {
			os << call.arguments.ordered[i].value;
			if (!call.arguments.labeled.empty() || i + 1 < call.arguments.ordered.size()) os << ", ";
		}
		for (size_t i = 0; i < call.arguments.labeled.size(); ++i) {
			os
				<< std::get<0>(call.arguments.labeled[i]).value
				<< ": "
				<< std::get<1>(call.arguments.labeled[i]).value;
			if (i + 1 < call.arguments.labeled.size()) os << ", ";
		}
		os << ')';
	}
	return os << ')';
}

std::ostream& operator<<(std::ostream& os, Expression const& expression) {
	switch (expression.kind()) {
	case Expression::Kind::Atom:            return os << expression.get_atom();
	case Expression::Kind::UnaryOperation:  return os << expression.get_unary_operation();
	case Expression::Kind::BinaryOperation: return os << expression.get_binary_operation();
	case Expression::Kind::FunctionCall:    return os << expression.get_function_call();
	}
}

std::ostream& operator<<(std::ostream& os, Type const& type) {
	switch (type.kind()) {
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

std::ostream& operator<<(std::ostream& os, Statement::Declare const& declare) {
	os << "[declare stmt: ";
	os << (declare.mutable_.value ? "mut" : "const") << " " << declare.name.value;
	if (declare.type.has_value()) os << ": " << declare.type.value().value;
	if (declare.value.has_value()) os << " = " << declare.value.value().value;
	return os << ";]";
}

std::ostream& operator<<(std::ostream& os, Statement::Set const& set) {
	os << "[set stmt: ";
	os << set.lhs.value << " = " << set.rhs.value;
	return os << ";]";
}

std::ostream& operator<<(std::ostream& os, Statement::Return const& return_) {
	os << "[return stmt: ";
	if (return_.value.has_value()) os << return_.value.value().value;
	else os << "(no value)";
	return os << ";]";
}

std::ostream& operator<<(std::ostream& os, Statement const& statement) {
	switch (statement.kind()) {
	case Statement::Kind::Declare:    return os << statement.get_declare();
	case Statement::Kind::Set:        return os << statement.get_set();
	case Statement::Kind::Expression: return os << "[expr stmt: " << statement.get_expression() << ";]";
	case Statement::Kind::Return:     return os << statement.get_return();
	case Statement::Kind::Scope:      break;
	}

	// FIXME: take indentation into account somehow
	Scope const& scope = statement.get_scope();
	os << "[scope stmt";
	if (scope.empty()) return os << " (empty)]";
	else os << ":\n";
	for (size_t i = 0; i < scope.size(); ++i) {
		os << '\t' << scope[i] << '\n';
	}
	return os << ']';
}

bool Expression::can_be_lhs() const {
	// TODO: change this once deref, member access exist

	if (kind() == Kind::FunctionCall) return true;
	if (kind() != Kind::Atom) return false;
	return get_atom().kind() == Atom::Kind::Identifier;
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

std::optional<Expression::FunctionCall::Argument> Parser::consume_expression_function_call_argument() {
	// an argument can either be <identifier>: <expr> or just <expr>, depending on whether it's named or
	// not. since a bare unqualified identifier is also an expression, we need to consume an expression and
	// then potentially consume a colon if the conditions are right, followed by another expression

	std::optional<Spanned<Expression>> argument_lhs = SPANNED(consume_expression);
	if (!argument_lhs.has_value()) return {};
	// check if it's a bare unqualified identifier followed by a colon
	if (argument_lhs.value().value.kind() == Expression::Kind::Atom
	    && argument_lhs.value().value.get_atom().kind() == Expression::Atom::Kind::Identifier
	    && argument_lhs.value().value.get_atom().get_identifier().is_unqualified()
	    && consume_symbol(Token::Symbol::Colon)) {
		// then, the bare unqualified identifier is the label
		Spanned<std::string_view> label
			= argument_lhs.value().value.get_atom().get_identifier().get_unqualified();
		// and we require an actual argument
		std::optional<Spanned<Expression>> argument_rhs
			= SPANNED_REASON(expect_expression, "expected argument value after argument label");
		// we don't really have anything to return if there's no argument
		if (!argument_rhs.has_value()) return {};
		return Expression::FunctionCall::LabeledArgument {label, std::move(argument_rhs.value())};
	}
	// otherwise it's just a regular argument
	return Expression::FunctionCall::OrderedArgument {std::move(argument_lhs.value())};
}

std::optional<Expression> Parser::consume_expression_function_call() {
	std::optional<Spanned<Expression>> maybe_callee = SPANNED(consume_expression_atom);
	if (!maybe_callee.has_value()) return {};
	Spanned<Expression> callee = std::move(maybe_callee.value());

	while (consume_symbol(Token::Symbol::LParen)) {
		std::optional<Expression::FunctionCall::Argument> argument {};
		Expression::FunctionCall::Arguments               arguments {};

		while ((argument = consume_expression_function_call_argument()).has_value()) {
			if (std::holds_alternative<Expression::FunctionCall::LabeledArgument>(argument.value())) {
				auto labeled_argument = std::move(
					std::get<Expression::FunctionCall::LabeledArgument>(argument.value())
				);
				arguments.labeled.push_back(std::move(labeled_argument));
			} else {
				auto ordered_argument = std::move(
					std::get<Expression::FunctionCall::OrderedArgument>(argument.value())
				);
				if (!arguments.labeled.empty()) {
					diagnostics_.push_back(
						Diagnostic::error(
							"cannot specify ordered argument after labeled argument(s)",
							"labeled arguments should be specified after all ordered arguments, so no ordered arguments can appear between the labeled arguments",
							{Diagnostic::Sample(ordered_argument.span)}
						)
					);
				} else {
					arguments.ordered.push_back(std::move(ordered_argument));
				}
			}

			while (peek_symbol(Token::Symbol::Comma)) tokens_.advance();
		}

		// we get the end of the span from the closing parenthesis which is the current token (unless it isn't)
		size_t end       = tokens_.has_value() ? tokens_.peek().value().span().end : tokens_.last().span().end;
		Span   call_span = Span(callee.span.start, end);

		// we do this before checking for closing parenthesis because it's still a function call even if you
		// forgot the closing parenthesis
		callee = Spanned<Expression> {
			call_span,
			Expression::make_function_call(
				std::make_unique<Spanned<Expression>>(std::move(callee)),
				std::move(arguments)
			)
		};

		if (!expect_symbol("expected closing parenthesis to end argument list", Token::Symbol::RParen)) break;
	}

	return std::move(callee.value);
}

// TODO: greatly improve this code with macros or some system that doesn't require this much repetition

std::optional<Expression> Parser::consume_expression_unary_l1() {
	// this refers to unary negation. this code is really ugly
	size_t negations = 0;
	while (consume_symbol(Token::Symbol::Minus)) negations++;
	std::optional<Spanned<Expression>> should_operand
		= negations
	                ? SPANNED_REASON(expect_expression_function_call, "expected expression after unary operator")
	                : SPANNED(consume_expression_function_call);
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

std::optional<Statement> Parser::consume_statement_declare() {
	// <"const"|"mut"> <name> [: <type>] [= <expr|"undefined">];

	// TODO: should we do something different for the "undefined" case?

	// we know we had to peek mut or const to get here
	bool mutable_value = peek_keyword(Keyword::Mut);
	Span mutable_span  = tokens_.peek().value().span();

	Spanned<bool> mutable_ {mutable_span, mutable_value};
	tokens_.advance();

	auto maybe_name = SPANNED_REASON(
		expect_identifier,
		"expected variable name after mutability qualifier to begin variable declaration"
	);
	if (!maybe_name.has_value()) return {};
	Spanned<std::string_view> name = maybe_name.value();

	std::optional<Spanned<Type>> type;
	if (consume_symbol(Token::Symbol::Colon)) {
		type = SPANNED_REASON(expect_type, "expected type after colon in variable declaration");
		if (!type.has_value()) return {};  // we don't want to keep parsing after that
	}

	std::optional<Spanned<Expression>> value;
	if (consume_symbol(Token::Symbol::Eq)) {
		value = SPANNED_REASON(
			expect_expression,
			"expected expression after equals sign in variable declaration"
		);
		// if there is no possible expression (perhaps a semicolon instead), a valueless stmt will be emitted as
		// fallback
	}

	expect_semicolon("expected semicolon after variable declaration");

	return Statement::make_declare(
		Statement::Declare {std::move(name), std::move(type), std::move(value), mutable_}
	);
}

std::optional<Statement> Parser::consume_statement_set() {
	// <(lvalue) expr> = <expr>;

	auto maybe_lhs = SPANNED(consume_expression);
	if (!maybe_lhs.has_value()) return {};
	Spanned<Expression> lhs = std::move(maybe_lhs.value());
	if (!consume_symbol(Token::Symbol::Eq)) return consume_statement_expression(std::move(lhs.value));
	if (!lhs.value.can_be_lhs())
		diagnostics_.push_back(
			Diagnostic::error(
				"invalid left-hand side for set statement",
				"set statements can only take lvalues (identifiers, function calls, dereferences and access expressions) in their left-hand side",
				{Diagnostic::Sample(lhs.span)}
			)
		);

	auto maybe_rhs = SPANNED_REASON(expect_expression, "expected expression after equals sign in set statement");
	if (!maybe_rhs.has_value()) return {};  // there is no reasonable fallback statement to emit
	Spanned<Expression> rhs = std::move(maybe_rhs.value());

	expect_semicolon("expected semicolon after variable declaration");

	return Statement::make_set(Statement::Set {std::move(lhs), std::move(rhs)});
}

std::optional<Statement> Parser::consume_statement_expression(Expression&& expression) {
	// <expr>;
	expect_semicolon("expected semicolon after expression statement");
	return Statement::make_expression(std::move(expression));
}

std::optional<Statement> Parser::consume_statement_return() {
	// "return" [expr];

	// we start after the "return" keyword
	auto value = SPANNED(consume_expression);

	expect_semicolon("expected semicolon after return statement");

	return Statement::make_return(Statement::Return {std::move(value)});
}

std::optional<Statement> Parser::consume_statement_scope() {
	// { [stmt1, stmt2, ...] }
	// we can delegate this to consume_scope
	return consume_scope().transform([](auto&& value) { return Statement::make_scope(std::move(value)); });
}

std::optional<Statement> Parser::consume_statement() {
	if (peek_symbol(Token::Symbol::LBrace)) return consume_statement_scope();

	// FIXME: if someone had a function or variable called const/mut/return, this would break!
	if (peek_keyword(Keyword::Const) || peek_keyword(Keyword::Mut)) { return consume_statement_declare(); }

	if (consume_keyword(Keyword::Return)) return consume_statement_return();

	return consume_statement_set();
}

std::optional<Scope> Parser::consume_scope() {
	if (!consume_symbol(Token::Symbol::LBrace)) return {};
	std::vector<Statement>   statements {};
	std::optional<Statement> statement {};
	skip_semis();
	while ((statement = consume_statement()).has_value()) {
		statements.push_back(std::move(statement.value()));
		skip_semis();
	}
	// we don't need to skip semis again
	expect_symbol("expected closing brace to end scope", Token::Symbol::RBrace);
	return Scope {std::move(statements)};
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
	case Keyword::Export: return token.get_identifier() == "export";
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

std::optional<Expression> Parser::expect_expression_function_call(std::string_view reason) {
	auto expression = consume_expression_function_call();
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

std::optional<Scope> Parser::expect_scope(std::string_view reason) {
	auto scope = consume_scope();
	if (scope.has_value()) return scope;
	Token last_token = tokens_.peek().value_or(tokens_.last());
	diagnostics_.push_back(
		Diagnostic::error("expected scope", std::string(reason), {Diagnostic::Sample(last_token.span())})
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
	return Module {name, std::move(body.value())};
}

std::optional<Module::Item> Parser::parse_module_item() {
	// if we find export, we consume it
	bool exported = consume_keyword(Keyword::Export);

	std::optional<Module::Item> item;
	if (peek_keyword(Keyword::Module)) {
		item = parse_module().transform([exported](auto&& value) {
			return Module::Item {exported, std::move(value)};
		});
	} else if (peek_keyword(Keyword::Func)) {
		item = parse_function().transform([exported](auto&& value) {
			return Module::Item {exported, std::move(value)};
		});
	}

	return item;
}

std::optional<Module::Body> Parser::parse_module_body(bool bare) {
	if (!bare)
		if (!expect_symbol("expected opening brace to begin module body", Token::Symbol::LBrace)) return {};
	skip_semis();
	std::vector<Spanned<Module::Item>> items {};
	for (auto item = SPANNED(parse_module_item); item.has_value(); item = SPANNED(parse_module_item)) {
		items.push_back(std::move(item.value()));
		skip_semis();
	}
	skip_semis();
	if (!bare) expect_symbol("expected closing brace to end module body", Token::Symbol::RBrace);
	return Module::Body {std::move(items)};
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

	std::optional<Scope> body {};
	if (consume_symbol(Token::Symbol::FatArrow)) {
		// if we get =>, we get an expression for the body, so we return it
		auto expression = SPANNED_REASON(
			expect_expression,
			"expected expression after => indicating function body shorthand"
		);

		if (expression.has_value()) {
			Statement statement = Statement::make_return(Statement::Return {std::move(expression.value())});
			// TODO: fix this, this is ugly, but i can't seem to directly construct the Scope
			body = Scope {};
			body.value().push_back(std::move(statement));
		}
	} else {
		// otherwise, we get a regular body
		body = consume_scope();
	}

	std::cout << "found function with name " << name.value().value << ", " << arguments.size() << " arguments: ";
	for (Function::Argument const& argument : arguments) {
		std::cout << argument.name.value << " of type " << argument.type.value << ", ";
	}
	std::cout << std::endl;
	if (body.has_value())
		for (auto const& stmt : body.value()) { std::cout << "\t" << stmt << std::endl; }

	return Function {name.value(), arguments, return_type, std::move(body)};
}

bool Parser::advance() {
	// FIXME: if some statement is misparsed, it keeps trying to advance and hangs
	if (!tokens_.has_value()) return false;  // EOF
	skip_semis();
	return parse_module_body(true).has_value();
}

}  // namespace AST
