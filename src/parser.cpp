#include "parser.hpp"

#include "lexer.hpp"

#include <cassert>
#include <charconv>
#include <format>
#include <sstream>
#include <string>
#include <string_view>

namespace AST {

#define SPANNED(fn) spanned((std::function<decltype((fn) ())()>) [&, this] { return (fn) (); })
// FIXME: clang-format won't stop jiggling this macro around LOL
#define SPANNED_REASON(fn, reason)                                                                                     \
	spanned((std::function<decltype((fn) (std::declval<decltype(reason)>()))()>) [&,                               \
		                                                                      this] { return (fn) (reason); })

Diagnostic ExpectedDiagnostic::as_diagnostic(FileContext const& context) const {
	std::stringstream title_stream {}, subtitle_stream {};
	title_stream << "expected ";
	// we know there should be at least one expectation
	assert(!expectations.empty());
	title_stream << expectations[0].what;
	subtitle_stream << expectations[0].why;
	if (expectations.size() == 1) goto done;
	// since there are more expectations, let's parenthesize the rest of them in the format (or A, B, C, D)
	title_stream << " (or ";
	for (size_t i = 1; i < expectations.size(); ++i) {
		title_stream << expectations[i].what;
		subtitle_stream << "\n\t(" << expectations[i].why << ")";
		// we can use expectations.size() - 1 because we know expectations.size() >= 1
		if (i + 1 < expectations.size() - 1) title_stream << ", ";
		else if (i < expectations.size() - 1) title_stream << " or ";
	}
	title_stream << ")";
done:
	std::string title    = title_stream.str();
	std::string subtitle = subtitle_stream.str();
	return Diagnostic::error(std::move(title), std::move(subtitle), {Diagnostic::Sample(context, where)});
}

std::vector<Diagnostic> Parser::diagnostics() const {
	std::vector<Diagnostic> diagnostics {};
	for (std::variant<ExpectedDiagnostic, Diagnostic> const& diagnostic : diagnostics_) {
		if (std::holds_alternative<Diagnostic>(diagnostic))
			diagnostics.push_back(std::get<Diagnostic>(diagnostic));
		else diagnostics.push_back(std::get<ExpectedDiagnostic>(diagnostic).as_diagnostic(context_));
	}
	return diagnostics;
}

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

bool Parser::consume_single_comma_or_more() {
	if (!consume_symbol(Token::Symbol::Comma)) return false;
	while (consume_symbol(Token::Symbol::Comma));
	return true;
}

std::optional<std::string> Parser::consume_label() {
	auto maybe_token = tokens_.peek();
	if (!maybe_token.has_value()) return {};
	Token token = maybe_token.value();
	if (!token.is_label()) return {};
	tokens_.advance();
	std::string label = token.get_label();
	// we must trim off the initial apostrophe
	return label.substr(1);
}

std::optional<std::string> Parser::consume_number_literal() {
	auto maybe_token = tokens_.peek();
	if (!maybe_token.has_value()) return {};
	Token token = maybe_token.value();
	if (!token.is_number_literal()) return {};
	tokens_.advance();
	return token.get_number_literal();
}

std::optional<std::string> Parser::consume_string_literal() {
	auto maybe_token = tokens_.peek();
	if (!maybe_token.has_value()) return {};
	Token token = maybe_token.value();
	if (!token.is_string_literal()) return {};
	tokens_.advance();
	return token.get_string_literal();
}

std::optional<std::string> Parser::consume_char_literal() {
	auto maybe_token = tokens_.peek();
	if (!maybe_token.has_value()) return {};
	Token token = maybe_token.value();
	if (!token.is_char_literal()) return {};
	tokens_.advance();
	return token.get_char_literal();
}

std::optional<std::string> Parser::consume_bare_unqualified_identifier() {
	auto maybe_token = tokens_.peek();
	if (!maybe_token.has_value()) return {};
	Token token = maybe_token.value();
	if (!token.is_identifier()) return {};
	tokens_.advance();
	return token.get_identifier();
}

std::optional<Identifier> Parser::consume_unqualified_identifier() {
	return SPANNED(consume_bare_unqualified_identifier).transform([](auto&& value) {
		return Identifier(std::move(value));
	});
}

std::optional<Identifier> Parser::consume_identifier() {
	// NOTE: in the future, we will have to account for static members (T::a) and potentially discarded identifiers
	// at the beginning (_::a)

	// if a :: can be consumed before the qualified identifier, it will be consumed and will turn the qualified
	// identifier absolute
	bool absolute = consume_symbol(Token::Symbol::ColonColon);
	// if :: was consumed, that means this is definitely and unambiguously a qualified identifier now
	std::optional<Spanned<std::string>> root
		= absolute ? SPANNED_REASON(
				     expect_bare_unqualified_identifier,
				     "expected an identifier (`::` starts an absolute qualified identifier)"
			     )
	                   : SPANNED(consume_bare_unqualified_identifier);
	// now if we didn't find the root we can safely return because, if :: has been parsed, we'll have skipped it :-)
	if (!root.has_value()) return {};
	std::vector<Spanned<std::string>> path {root.value()};
	while (consume_symbol(Token::Symbol::ColonColon)) {
		std::optional<Spanned<std::string>> piece = SPANNED_REASON(
			expect_bare_unqualified_identifier,
			"expected an identifier (there is a trailing `::` in a preceding qualified identifier)"
		);
		if (!piece.has_value()) return Identifier(absolute, std::move(path));
		path.push_back(piece.value());
	}
	return Identifier(absolute, std::move(path));
}

std::optional<Tag> Parser::consume_tag() {
	if (!consume_symbol(Token::Symbol::At)) return {};
	std::optional<std::string> name
		= expect_bare_unqualified_identifier("expected an identifier (`@` starts a tag)");
	if (!name.has_value()) return {};
	return Tag {name.value()};
}

std::optional<Type> Parser::consume_type_atom() {
	std::optional<Spanned<Identifier>> maybe_name = SPANNED(consume_identifier);
	if (!maybe_name.has_value()) return {};

	// if it's a qualified identifier, it's definitely named
	if (!maybe_name.value().value.is_unqualified()) {
		return Type::make_atom(Type::Atom::make_named(Type::Atom::Named {std::move(maybe_name.value())}));
	}

	// if it's not a qualified identifier, it's a built-in or a named one in scope
	std::string name = maybe_name.value().value.name();

	// variables have to declared at the top so c++ won't wail
	bool                               starts_with_u, starts_with_i;
	uint32_t                           width;
	std::from_chars_result             res;
	std::optional<Type::Atom::Integer> int_ {};
	size_t                             base_length = 0;

	// "easy" types
	if (name == "void") return Type::make_atom(Type::Atom::make_void());
	if (name == "char") return Type::make_atom(Type::Atom::make_char());
	if (name == "bool") return Type::make_atom(Type::Atom::make_bool());
	if (name == "_") return Type::make_atom(Type::Atom::make_inferred());

	// float types (can be bruteforced)
	if (name.starts_with("float")) {
		// TODO: support arbitrary sized floats (should we?)
		if (name == "float16") return Type::make_atom(Type::Atom::make_float(Type::Atom::Float::Width::F16));
		if (name == "float32") return Type::make_atom(Type::Atom::make_float(Type::Atom::Float::Width::F32));
		if (name == "float64") return Type::make_atom(Type::Atom::make_float(Type::Atom::Float::Width::F64));
		if (name == "float128") return Type::make_atom(Type::Atom::make_float(Type::Atom::Float::Width::F128));
		goto none_match;
	}

	// just the integer types left
	starts_with_u = name.starts_with("uint");
	starts_with_i = name.starts_with("int");
	if (!starts_with_u && !starts_with_i) goto none_match;
	// get rid of the easy cases
	base_length = starts_with_u ? 4 : 3;
	if (name.length() == base_length)
		return Type::make_atom(Type::Atom::make_integer(Type::Atom::Integer::any(starts_with_i)));
	if (name.length() == base_length + 3 && name.ends_with("ptr"))
		return Type::make_atom(Type::Atom::make_integer(Type::Atom::Integer::ptr(starts_with_i)));
	if (name.length() == base_length + 4 && name.ends_with("size"))
		return Type::make_atom(Type::Atom::make_integer(Type::Atom::Integer::size(starts_with_i)));
	// we know the name is at least base_length long, so the following is valid
	res = std::from_chars(name.data() + base_length, name.data() + name.size(), width);
	if (res.ec != std::errc() || res.ptr != name.data() + name.size()) goto none_match;
	int_ = Type::Atom::Integer::with_width(width, starts_with_i);
	if (!int_.has_value()) goto none_match;
	return Type::make_atom(Type::Atom::make_integer(std::move(int_.value())));

none_match:
	// if none match, it's once more a named type
	return Type::make_atom(Type::Atom::make_named(Type::Atom::Named {std::move(maybe_name.value())}));
}

std::optional<Type> Parser::consume_type() {
	// for now, we only have pointers, so let's deal with those
	std::vector<Spanned<bool>> operations {};
	while (peek_symbol(Token::Symbol::Star)) {
		Span span = tokens_.peek().value().span();
		tokens_.advance();
		bool mutable_ = consume_keyword(Keyword::Mut);
		if (!mutable_)
			expect_keyword("expected mutability qualifier after pointer symbol in type", Keyword::Const);
		operations.push_back({span, mutable_});
	}

	// if there are no operations, we just need to parse a consume instance
	if (operations.empty()) return consume_type_atom();

	// we must iterate over them in reverse order.
	std::optional<Spanned<Type>> should_operand
		= SPANNED_REASON(expect_type_atom, "expected type after pointer symbol");
	if (!should_operand.has_value()) return {};
	Spanned<Type> operand = std::move(should_operand.value());
	for (size_t i = operations.size(); i > 0; --i) {
		Spanned<bool> operation = std::move(operations.at(i - 1));

		operand = Spanned<Type> {
			Span(operation.span.start, operand.span.end),
			Type::make_pointer(
				Type::Pointer {std::make_unique<Spanned<Type>>(std::move(operand)), operation.value}
			)
		};
	}

	return std::move(operand.value);
}

std::optional<Expression::Atom::StructLiteral::Field> Parser::consume_expression_struct_literal_field() {
	// this is just <name> ":" <value expr>

	auto name = SPANNED(consume_bare_unqualified_identifier);
	if (!name.has_value()) return {};
	if (!expect_symbol("expected `:` after struct literal field name", Token::Symbol::Colon)) return {};
	auto value = SPANNED_REASON(expect_expression, "expected struct literal field value after `:`");
	if (!value.has_value()) return {};

	return {
		{std::move(name.value()), std::make_unique<Spanned<Expression>>(std::move(value.value()))}
	};
}

std::optional<Expression> Parser::consume_expression_atom() {
	// it could be a potentially qualified identifier
	std::optional<Spanned<Identifier>> identifier = SPANNED(consume_identifier);
	if (identifier.has_value()) {
		// special case for "true" and "false", which refer to boolean literals. they are strict keywords in
		// this sense
		if (identifier.value().value.is_unqualified()
		    && (identifier.value().value.name() == "true" || identifier.value().value.name() == "false")) {
			return Expression::make_atom(
				Expression::Atom::make_bool_literal(identifier.value().value.name() == "true")
			);
		}

		// special case for struct literals
		if (peek_symbol(Token::Symbol::Lt) || peek_symbol(Token::Symbol::LBrace)) {
			size_t                     index        = tokens_.index();
			std::optional<GenericList> generic_list = std::nullopt;
			// this might still not be a struct literal, but a function call!
			// we check it here because the function call calls this for its callee, meaning that the
			// function call won't check for generics before we do.
			if (peek_symbol(Token::Symbol::Lt)) {
				// we don't have to check for whitespace, because there's no reason why you would do
				// A<B, C>{...} since there are no scope expressions :-)
				generic_list = consume_generic_list();
				// we bail if it's invalid
				if (!generic_list.has_value() || !peek_symbol(Token::Symbol::LBrace)) {
					tokens_.set_index(index);
					goto bail;
				}
			}
			assert(consume_symbol(Token::Symbol::LBrace));
			std::optional<Expression::Atom::StructLiteral::Field> maybe_field;
			std::vector<Expression::Atom::StructLiteral::Field>   fields {};

			while ((maybe_field = consume_expression_struct_literal_field()).has_value()) {
				Expression::Atom::StructLiteral::Field field = std::move(maybe_field.value());

				auto duplicate_field = std::find_if(
					fields.cbegin(),
					fields.cend(),
					[&field](Expression::Atom::StructLiteral::Field const& given_field) {
						return given_field.name.value == field.name.value;
					}
				);

				if (duplicate_field != fields.cend()) {
					diagnostics_.push_back(
						Diagnostic::error(
							"duplicate field name",
							"field name used twice in struct literal",
							{Diagnostic::Sample(
								context_,
								{Diagnostic::Sample::Label(
									 field.name.span,
									 OutFmt::Color::Red
								 ),
					                         Diagnostic::Sample::Label(
									 duplicate_field->name.span,
									 "first used here",
									 OutFmt::Color::Cyan
								 )}
							)}
						)
					);
				} else {
					// we only push it if it's not duplicate
					fields.push_back(std::move(field));
				}
				if (!consume_single_comma_or_more()) break;
			}

			expect_symbol("expected closing brace to end struct literal", Token::Symbol::RBrace);

			return Expression::make_atom(
				Expression::Atom::make_struct_literal(
					std::move(identifier.value()),
					std::move(generic_list),
					std::move(fields)
				)
			);
		}

	bail:
		return Expression::make_atom(Expression::Atom::make_identifier(std::move(identifier.value().value)));
	}

	// since it's not an identifier, it has to be a literal
	if (!tokens_.has_value()) return {};
	// we only want to accept suffix literals if they're right beside the current token
	Span                       current_span = tokens_.peek().value().span();
	std::optional<std::string> sv;
	std::optional<Identifier>  suffix;  // we always try to consume an extra suffix

	if (sv = consume_number_literal(), sv.has_value()) {
		if (tokens_.peek().has_value() && tokens_.peek().value().span().start == current_span.end)
			suffix = consume_unqualified_identifier();
		return Expression::make_atom(Expression::Atom::make_number_literal(sv.value(), suffix));
	}

	if (sv = consume_string_literal(), sv.has_value()) {
		if (tokens_.peek().has_value() && tokens_.peek().value().span().start == current_span.end)
			suffix = consume_unqualified_identifier();
		return Expression::make_atom(Expression::Atom::make_string_literal(sv.value(), suffix));
	}

	if (sv = consume_char_literal(), sv.has_value()) {
		if (tokens_.peek().has_value() && tokens_.peek().value().span().start == current_span.end)
			suffix = consume_unqualified_identifier();
		return Expression::make_atom(Expression::Atom::make_char_literal(sv.value(), suffix));
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
	if (argument_lhs.value().value.is_atom()
	    && argument_lhs.value().value.get_atom().is_identifier()
	    && argument_lhs.value().value.get_atom().get_identifier().is_unqualified()
	    && consume_symbol(Token::Symbol::Colon)) {
		// then, the bare unqualified identifier is the label
		Spanned<Identifier> label
			= argument_lhs.value().value.get_atom().get_identifier().extract_unqualified_with_span();
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

	while (peek_symbol(Token::Symbol::Lt) || peek_symbol(Token::Symbol::LParen)) {
		std::optional<GenericList> generic_list = std::nullopt;
		// if we got a generic list, handle it and get us past the opening parenthesis
		if (peek_symbol(Token::Symbol::Lt)) {
			// disambiguate via whitespace :o
			if (callee.span.end != tokens_.peek().value().span().start) break;
			generic_list = expect_generic_list(
				"expected generic list after opening angle bracket (if you wanted a comparison operator, add a space before `<`)"
			);
			if (!generic_list.has_value()) break;
			if (!expect_symbol(
				    "expected opening parenthesis to begin argument list",
				    Token::Symbol::LParen
			    ))
				break;
		} else consume_symbol(Token::Symbol::LParen);
		std::optional<Expression::FunctionCall::Argument> argument {};
		Expression::FunctionCall::Arguments               arguments {};

		while ((argument = consume_expression_function_call_argument()).has_value()) {
			if (std::holds_alternative<Expression::FunctionCall::LabeledArgument>(argument.value())) {
				auto labeled_argument = std::move(
					std::get<Expression::FunctionCall::LabeledArgument>(argument.value())
				);

				auto duplicate_argument = std::find_if(
					arguments.labeled.cbegin(),
					arguments.labeled.cend(),
					[&labeled_argument](auto const& existing_argument) {
						return std::get<0>(existing_argument).value.name()
					            == std::get<0>(labeled_argument).value.name();
					}
				);

				if (duplicate_argument != arguments.labeled.cend()) {
					diagnostics_.push_back(
						Diagnostic::error(
							"duplicate argument name",
							"argument name used twice in function call",
							{Diagnostic::Sample(
								context_,
								{Diagnostic::Sample::Label(
									 std::get<0>(labeled_argument).span,
									 OutFmt::Color::Red
								 ),
					                         Diagnostic::Sample::Label(
									 std::get<0>(*duplicate_argument).span,
									 "first used here",
									 OutFmt::Color::Cyan
								 )}
							)}
						)
					);
				} else {
					// we only push the argument if it's not duplicate to avoid confusing later
					// stages
					arguments.labeled.push_back(std::move(labeled_argument));
				}

			} else {
				auto ordered_argument = std::move(
					std::get<Expression::FunctionCall::OrderedArgument>(argument.value())
				);
				if (!arguments.labeled.empty()) {
					diagnostics_.push_back(
						Diagnostic::error(
							"cannot specify ordered argument after labeled argument(s)",
							"labeled arguments should be specified after all ordered arguments, so no ordered arguments can appear between the labeled arguments",
							{Diagnostic::Sample(context_, ordered_argument.span)}
						)
					);
				} else {
					arguments.ordered.push_back(std::move(ordered_argument));
				}
			}

			if (!consume_single_comma_or_more()) break;
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
				std::move(arguments),
				std::move(generic_list)
			)
		};

		if (!expect_symbol("expected closing parenthesis to end argument list", Token::Symbol::RParen)) break;
	}

	return std::move(callee.value);
}

std::optional<Expression> Parser::consume_expression_member_access() {
	std::optional<Spanned<Expression>> maybe_accessee = SPANNED(consume_expression_function_call);
	if (!maybe_accessee.has_value()) return {};
	Spanned<Expression> accessee = std::move(maybe_accessee.value());

	while (consume_symbol(Token::Symbol::Dot)) {
		auto maybe_field = SPANNED_REASON(expect_bare_unqualified_identifier, "expected field name after dot");
		if (!maybe_field) return std::move(accessee.value);
		auto field = std::move(maybe_field.value());

		Span span(accessee.span.start, field.span.end);

		accessee = Spanned<Expression> {
			span,
			Expression::make_member_access(
				std::make_unique<Spanned<Expression>>(std::move(accessee)),
				std::move(field)
			)
		};
	}

	return std::move(accessee.value);
}

std::optional<Expression> Parser::consume_generic_binop(
	std::optional<Expression> (Parser::*consume)(),
	std::optional<Expression> (Parser::*expect)(std::string_view),
	std::vector<Token::Symbol>&& operators
) {
	std::optional<Spanned<Expression>> maybe_lhs = SPANNED(this->*consume);
	if (!maybe_lhs.has_value()) return {};
	Spanned<Expression> lhs = std::move(maybe_lhs.value());

	std::optional<Token::Symbol> operator_;
	while ((operator_ = peek_symbols(operators)).has_value()) {
		tokens_.advance();

		std::optional<Spanned<Expression>> should_rhs
			= SPANNED_REASON(this->*expect, "expected expression after binary operator");
		if (!should_rhs.has_value()) return std::move(lhs.value);  // error recovery
		Spanned<Expression> rhs = std::move(should_rhs.value());

		lhs = Spanned<Expression> {
			Span(lhs.span.start, rhs.span.end),
			Expression::make_binary_operation(
				std::make_unique<Spanned<Expression>>(std::move(lhs)),
				std::make_unique<Spanned<Expression>>(std::move(rhs)),
				operator_.value()
			)
		};
	}

	return std::move(lhs.value);
}

std::optional<Expression> Parser::consume_generic_unop(
	std::optional<Expression> (Parser::*consume)(),
	std::optional<Expression> (Parser::*expect)(std::string_view),
	std::vector<Token::Symbol>&& operators
) {
	std::vector<std::variant<Spanned<Token::Symbol>, Spanned<bool>>> operations {};
	std::optional<Token::Symbol>                                     operator_;
	while ((operator_ = peek_symbols(operators)).has_value()) {
		Span span = tokens_.peek().value().span();
		tokens_.advance();
		if (operator_.value() == Token::Symbol::Amp) {
			if (!tokens_.has_value()) return {};
			span.end      = tokens_.peek().value().span().end;
			bool mutable_ = consume_keyword(Keyword::Mut);
			if (!mutable_)
				expect_keyword("expected mutability qualifier after address operator", Keyword::Const);
			operations.push_back(Spanned {span, mutable_});
			continue;
		}
		operations.push_back(Spanned {span, operator_.value()});
	}

	// if there are no operations, we just need to parse a consume instance
	if (operations.empty()) return (this->*consume)();

	// we must iterate over them in reverse order: ABC<expr> needs to become A(B(C(<expr>))).
	std::optional<Spanned<Expression>> should_operand
		= SPANNED_REASON(this->*expect, "expected expression after unary operator");
	if (!should_operand.has_value()) return {};
	Spanned<Expression> operand = std::move(should_operand.value());
	for (size_t i = operations.size(); i > 0; --i) {
		if (std::holds_alternative<Spanned<bool>>(operations.at(i - 1))) {
			Spanned<bool> operation = std::move(std::get<Spanned<bool>>(operations.at(i - 1)));
			operand                 = Spanned<Expression> {
                                Span(operation.span.start, operand.span.end),
                                Expression::make_address_operation(
                                        std::make_unique<Spanned<Expression>>(std::move(operand)),
                                        operation.value
                                )
                        };
			continue;
		}

		Spanned<Token::Symbol> operation = std::move(std::get<Spanned<Token::Symbol>>(operations.at(i - 1)));

		operand = Spanned<Expression> {
			Span(operation.span.start, operand.span.end),
			Expression::make_unary_operation(
				std::make_unique<Spanned<Expression>>(std::move(operand)),
				operation.value
			)
		};
	}

	return std::move(operand.value);
}

std::optional<Expression> Parser::consume_expression_unary_l1() {
	// then, unary -, !, & and *
	return consume_generic_unop(
		&Parser::consume_expression_member_access,
		&Parser::expect_expression_member_access,
		{Token::Symbol::Minus, Token::Symbol::Bang, Token::Symbol::Amp, Token::Symbol::Star}
	);
}

std::optional<Expression> Parser::consume_expression_binop_l1() {
	// then *, /
	return consume_generic_binop(
		&Parser::consume_expression_unary_l1,
		&Parser::expect_expression_unary_l1,
		{Token::Symbol::Star, Token::Symbol::Div}
	);
}

std::optional<Expression> Parser::consume_expression_binop_l2() {
	// then +, -
	return consume_generic_binop(
		&Parser::consume_expression_binop_l1,
		&Parser::expect_expression_binop_l1,
		{Token::Symbol::Plus, Token::Symbol::Minus}
	);
}

std::optional<Expression> Parser::consume_expression_binop_l3() {
	// then comparison <, >, <=, >=, ==, !=
	return consume_generic_binop(
		&Parser::consume_expression_binop_l2,
		&Parser::expect_expression_binop_l2,
		{Token::Symbol::Lt,
	         Token::Symbol::Gt,
	         Token::Symbol::Le,
	         Token::Symbol::Ge,
	         Token::Symbol::EqEq,
	         Token::Symbol::Ne}
	);
}

std::optional<Expression> Parser::consume_expression_binop_l4() {
	// then logical &&, ||
	return consume_generic_binop(
		&Parser::consume_expression_binop_l3,
		&Parser::expect_expression_binop_l3,
		{Token::Symbol::AmpAmp, Token::Symbol::BarBar}
	);
}

std::optional<Expression> Parser::consume_expression() {
	size_t index = tokens_.index();
	if (!consume_keyword(Keyword::If)) return consume_expression_binop_l4();
	if (!consume_symbol(Token::Symbol::LParen)) {
		// we might have a variable called if
		tokens_.set_index(index);
		return consume_expression_binop_l4();
	}
	auto maybe_condition = SPANNED_REASON(expect_expression, "expected condition expression for if expression");
	if (!maybe_condition.has_value()) return {};
	expect_symbol("expected closing parenthesis after if expression condition expression", Token::Symbol::RParen);
	auto maybe_true
		= SPANNED_REASON(expect_expression, "expected expression after if expression condition expression");
	if (!maybe_true.has_value()) {
		// this could mean that there is a function called "if". in that case though , i'm sorry :/
		return {};
	}
	expect_keyword("expected `else` clause after `if` clause in if expression", Keyword::Else);
	auto maybe_false = SPANNED_REASON(expect_expression, "expected expression after else keyword in if expression");
	if (!maybe_false.has_value()) return {};
	return Expression::make_if(
		std::make_unique<Spanned<Expression>>(std::move(maybe_condition.value())),
		std::make_unique<Spanned<Expression>>(std::move(maybe_true.value())),
		std::make_unique<Spanned<Expression>>(std::move(maybe_false.value()))
	);
}

std::optional<GenericDeclaration::Generic> Parser::consume_generic_declaration_generic() {
	// ["anon" <name>]
	auto generic_name = SPANNED(consume_unqualified_identifier);
	if (!generic_name.has_value()) return {};

	// name might actually just be anon
	bool anonymous = false;
	if (generic_name.value().value.name() == "anon" && peek_unqualified_identifier()) {
		anonymous    = true;
		generic_name = SPANNED(consume_unqualified_identifier);
	}

	return GenericDeclaration::Generic {std::move(generic_name.value()), anonymous};
}

std::optional<GenericDeclaration> Parser::consume_generic_declaration() {
	// "<" <generic> ["," <generic>]* ">"
	if (!consume_symbol(Token::Symbol::Lt)) return {};
	std::vector<GenericDeclaration::Generic> generics {};

	std::optional<GenericDeclaration::Generic> generic;
	while ((generic = consume_generic_declaration_generic()).has_value()) {
		auto generic_name      = generic.value().name.value.name();
		auto duplicate_generic = std::find_if(
			generics.cbegin(),
			generics.cend(),
			[&generic_name](GenericDeclaration::Generic const& given_generic) {
				return given_generic.name.value.name() == generic_name;
			}
		);

		if (duplicate_generic != generics.cend()) {
			diagnostics_.push_back(
				Diagnostic::error(
					"duplicate generic name",
					"generic name used twice in generic declaration",
					{Diagnostic::Sample(
						context_,
						{Diagnostic::Sample::Label(
							 generic.value().name.span,
							 OutFmt::Color::Red
						 ),
			                         Diagnostic::Sample::Label(
							 duplicate_generic->name.span,
							 "first used here",
							 OutFmt::Color::Cyan
						 )}
					)}
				)
			);
		} else {
			// we only push the generic if it's not duplicate to avoid confusing later stages
			generics.push_back(std::move(generic.value()));
		}

		if (!consume_single_comma_or_more()) break;
	}

	expect_symbol("expected closing angle bracket to end generic declaration", Token::Symbol::Gt);
	return GenericDeclaration {std::move(generics)};
}

std::optional<GenericList::Generic> Parser::consume_generic_list_generic() {
	// <type> or <name> ":" <type>
	std::optional<Spanned<Type>> generic_lhs = SPANNED(consume_type);
	if (!generic_lhs.has_value()) return {};
	// check if it's a bare unqualified identifier followed by a colon
	// TODO: also check that there are no generics
	if (generic_lhs.value().value.is_atom()
	    && generic_lhs.value().value.get_atom().is_named()
	    && generic_lhs.value().value.get_atom().get_named().name.value.is_unqualified()
	    && consume_symbol(Token::Symbol::Colon)) {
		Spanned<std::string> label {
			generic_lhs.value().span,
			generic_lhs.value().value.get_atom().get_named().name.value.name()
		};
		// now we need the actual generic
		std::optional<Spanned<Type>> generic_rhs
			= SPANNED_REASON(expect_type, "expected type after generic label");
		// we don't really have anything to return if there's no generic
		if (!generic_rhs.has_value()) return {};
		return GenericList::LabeledGeneric {std::move(label), std::move(generic_rhs.value())};
	}
	// otherwise it's just a regular generic
	return GenericList::OrderedGeneric {std::move(generic_lhs.value())};
}

std::optional<GenericList> Parser::consume_generic_list() {
	// "<" <generic> ["," <generic>]* ">"
	if (!consume_symbol(Token::Symbol::Lt)) return {};
	std::optional<GenericList::Generic> generic {};

	// TODO: might be useful to be able to quit with an error here so the expect_ method makes sense :P
	std::vector<GenericList::LabeledGeneric> labeled_generics {};
	std::vector<GenericList::OrderedGeneric> ordered_generics {};

	while ((generic = consume_generic_list_generic()).has_value()) {
		if (std::holds_alternative<GenericList::LabeledGeneric>(generic.value())) {
			auto labeled_generic = std::move(std::get<GenericList::LabeledGeneric>(generic.value()));

			auto duplicate_generic = std::find_if(
				labeled_generics.cbegin(),
				labeled_generics.cend(),
				[&labeled_generic](auto const& existing_generic) {
					return std::get<0>(existing_generic).value
				            == std::get<0>(labeled_generic).value;
				}
			);

			if (duplicate_generic != labeled_generics.cend()) {
				diagnostics_.push_back(
					Diagnostic::error(
						"duplicate generic name",
						"generic name used twice in generic list",
						{Diagnostic::Sample(
							context_,
							{Diagnostic::Sample::Label(
								 std::get<0>(labeled_generic).span,
								 OutFmt::Color::Red
							 ),
				                         Diagnostic::Sample::Label(
								 std::get<0>(*duplicate_generic).span,
								 "first used here",
								 OutFmt::Color::Cyan
							 )}
						)}
					)
				);
			} else {
				// we only push the generic if it's not duplicate to avoid confusing later
				// stages
				labeled_generics.push_back(std::move(labeled_generic));
			}
		} else {
			auto ordered_generic = std::move(std::get<GenericList::OrderedGeneric>(generic.value()));
			if (!labeled_generics.empty()) {
				diagnostics_.push_back(
					Diagnostic::error(
						"cannot specify ordered generic after labeled generic(s)",
						"labeled generics should be specified after all ordered generics, so no ordered generics can appear between the labeled generics",
						{Diagnostic::Sample(context_, ordered_generic.span)}
					)
				);
			} else {
				ordered_generics.push_back(std::move(ordered_generic));
			}
		}
		if (!consume_single_comma_or_more()) break;
	}

	expect_symbol("expected closing angle bracket to end generic list", Token::Symbol::Gt);
	return GenericList {std::move(ordered_generics), std::move(labeled_generics)};
}

std::optional<Statement> Parser::consume_statement_declare() {
	// <"const"|"mut"> <name> [: <type>] [= <expr|"undefined">];

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
	Spanned<Identifier> name = maybe_name.value();

	std::optional<Spanned<Type>> type;
	if (consume_symbol(Token::Symbol::Colon)) {
		type = SPANNED_REASON(expect_type, "expected type after colon in variable declaration");
		if (!type.has_value()) return {};  // we don't want to keep parsing after that
	}

	std::optional<Spanned<Expression>> value;
	bool                               is_undefined = false;
	if (consume_symbol(Token::Symbol::Eq)) {
		value = SPANNED_REASON(
			expect_expression,
			"expected expression after equals sign in variable declaration"
		);
		// if the value is just "undefined", this corresponds to the undefined case
		if (value.has_value()
		    && value.value().value.is_atom()
		    && value.value().value.get_atom().is_identifier()
		    && value.value().value.get_atom().get_identifier().is_unqualified()
		    && value.value().value.get_atom().get_identifier().name() == "undefined") {
			value        = {};
			is_undefined = true;
		}
		// if there is no possible expression (perhaps a semicolon instead), a valueless stmt will be emitted as
		// fallback
	}

	expect_semicolon("expected semicolon after variable declaration");

	return Statement::make_declare(
		Statement::Declare {std::move(name), std::move(type), std::move(value), mutable_, is_undefined}
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
				"set statements can only take lvalues (identifiers, dereferences and access expressions) in their left-hand side",
				{Diagnostic::Sample(context_, lhs.span)}
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

std::optional<Statement> Parser::consume_statement_label() {
	// <label>:
	auto maybe_label = consume_label();
	if (!maybe_label.has_value()) return {};
	auto label = maybe_label.value();
	expect_symbol("expected colon after label", Token::Symbol::Colon);
	return Statement::make_label(Statement::Label {std::move(label)});
}

std::optional<Statement> Parser::consume_statement_goto() {
	// "goto" <label>;

	// we start after the "goto" keyword
	auto maybe_label = expect_label("expected label after goto keyword");
	if (!maybe_label.has_value()) return {};
	auto label = maybe_label.value();
	expect_semicolon("expected semicolon after goto statement");
	return Statement::make_goto(Statement::Goto {std::move(label)});
}

std::optional<Statement> Parser::consume_statement_branch() {
	// "branch" "(" <expr> ")" <true_label> [false_label];

	// we start after the "branch" keyword
	// TODO: should we be this liberal with the parentheses?
	expect_symbol("expected opening parenthesis after branch keyword", Token::Symbol::LParen);
	auto maybe_expression = SPANNED_REASON(expect_expression, "expected condition expression for branch statement");
	if (!maybe_expression.has_value()) return {};
	expect_symbol("expected closing parenthesis after branch condition expression", Token::Symbol::RParen);
	auto maybe_label1 = SPANNED_REASON(expect_label, "expected label after branch condition expression");
	if (!maybe_label1.has_value()) return {};
	auto maybe_label2 = SPANNED(consume_label);
	expect_semicolon("expected semicolon after branch statement");
	Spanned<Statement::Goto>                true_ {maybe_label1.value().span, {maybe_label1.value().value}};
	std::optional<Spanned<Statement::Goto>> false_ = maybe_label2.transform([](auto&& value) {
		return Spanned<Statement::Goto> {value.span, Statement::Goto {value.value}};
	});
	return Statement::make_branch(Statement::Branch {std::move(maybe_expression.value()), true_, false_});
}

std::optional<Statement> Parser::consume_statement_if() {
	// "if" "(" <expr> ")" <true_stmt> ["else" <false_stmt>]

	// we start after the "if" keyword
	// TODO: should we be this liberal with the parentheses?
	expect_symbol("expected opening parenthesis after if keyword", Token::Symbol::LParen);
	auto maybe_expression = SPANNED_REASON(expect_expression, "expected condition expression for if statement");
	if (!maybe_expression.has_value()) return {};
	expect_symbol("expected closing parenthesis after if condition expression", Token::Symbol::RParen);
	// if these stmts are scopes, they are flattened into the scope that we store in AST::Statement::If
	auto maybe_true = SPANNED_REASON(expect_statement, "expected scope or statement after if condition expression");
	if (!maybe_true.has_value()) return {};
	std::optional<Spanned<AST::Statement>> maybe_false = std::nullopt;
	if (consume_keyword(Keyword::Else)) {
		maybe_false = SPANNED_REASON(expect_statement, "expected scope or statement after else keyword");
		// we can survive if the user inputs else but not a statement
	}
	Spanned<Scope> true_scope {maybe_true.value().span, {}};
	// we have to do it this way because the brace initializer list constructor for vectors copies
	if (maybe_true.value().value.is_scope()) true_scope.value = std::move(maybe_true.value().value.get_scope());
	else true_scope.value.push_back(std::move(maybe_true.value()));
	std::optional<Spanned<Scope>> false_scope = std::nullopt;
	if (maybe_false.has_value()) {
		false_scope = {maybe_false.value().span, {}};
		if (maybe_false.value().value.is_scope())
			false_scope.value().value = std::move(maybe_false.value().value.get_scope());
		else false_scope.value().value.push_back(std::move(maybe_false.value()));
	}
	return AST::Statement::make_if(
		AST::Statement::If {std::move(maybe_expression.value()), std::move(true_scope), std::move(false_scope)}
	);
}

std::optional<Statement> Parser::consume_statement() {
	if (peek_symbol(Token::Symbol::LBrace)) return consume_statement_scope();

	if (peek_label()) return consume_statement_label();

	// FIXME: if someone had a function or variable called const/mut/return/goto, this would break!

	if (peek_keyword(Keyword::Const) || peek_keyword(Keyword::Mut)) { return consume_statement_declare(); }
	if (consume_keyword(Keyword::Return)) return consume_statement_return();
	if (consume_keyword(Keyword::Goto)) return consume_statement_goto();
	if (consume_keyword(Keyword::Branch)) return consume_statement_branch();
	if (consume_keyword(Keyword::If)) return consume_statement_if();

	return consume_statement_set();
}

std::optional<Scope> Parser::consume_scope() {
	if (!consume_symbol(Token::Symbol::LBrace)) return {};
	std::vector<Spanned<Statement>>   statements {};
	std::optional<Spanned<Statement>> statement {};
	skip_semis();
	while ((statement = SPANNED(consume_statement)).has_value()) {
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

std::optional<Token::Symbol> Parser::peek_symbols(std::vector<Token::Symbol> const& symbols) const {
	auto symbol = std::find_if(symbols.cbegin(), symbols.cend(), [this](Token::Symbol symbol) {
		return peek_symbol(symbol);
	});
	return (symbol == symbols.cend()) ? std::nullopt : std::optional {*symbol};
}

bool Parser::peek_keyword(Keyword keyword) const {
	auto maybe_token = tokens_.peek();
	if (!maybe_token.has_value()) return false;
	Token token = maybe_token.value();
	if (!token.is_identifier()) return false;
	return token.get_identifier() == get_variant_name(keyword);
}

bool Parser::peek_unqualified_identifier() const {
	auto maybe_token = tokens_.peek();
	if (!maybe_token.has_value()) return false;
	return maybe_token.value().is_identifier();
}

bool Parser::peek_label() const {
	auto maybe_token = tokens_.peek();
	if (!maybe_token.has_value()) return false;
	return maybe_token.value().is_label();
}

// TODO: add more context to some expected diagnoses (e.g. maybe for closing brace, add the header of what we want to
// close?). also because some of these stop making sense (e.g. if it wants an argument type but doesn't find it, it goes
// up and ends up expecting module closure or something)

#define EXPECT(fn, what)                                               \
	auto maybe = fn();                                             \
	if (!maybe.has_value()) add_expected_diagnostic(what, reason); \
	return maybe

void Parser::add_expected_diagnostic(std::string_view what, std::string_view why) {
	Token last_token = tokens_.peek().value_or(tokens_.last());

	if (!diagnostics_.empty()
	    && std::holds_alternative<ExpectedDiagnostic>(diagnostics_.at(diagnostics_.size() - 1))) {
		ExpectedDiagnostic& expected_diagnostic
			= std::get<ExpectedDiagnostic>(diagnostics_.at(diagnostics_.size() - 1));
		if (expected_diagnostic.where == last_token.span()) {
			ExpectedDiagnostic::Expectation expectation {std::string(what), std::string(why)};
			// only push the expectation if we don't already have it
			if (!expected_diagnostic.has_expectation(expectation))
				expected_diagnostic.expectations.push_back({std::string(what), std::string(why)});
			return;
		}
		// otherwise, push a new expected diagnostic
	}
	diagnostics_.push_back(ExpectedDiagnostic {{{std::string(what), std::string(why)}}, last_token.span()});
}

// it's better to keep these ones expanded because of how specific they are
bool Parser::expect_keyword(std::string_view reason, Keyword keyword) {
	if (consume_keyword(keyword)) return true;
	add_expected_diagnostic(std::format("keyword `{}`", get_variant_name(keyword)), reason);
	return false;
}

bool Parser::expect_symbol(std::string_view reason, Token::Symbol symbol) {
	if (consume_symbol(symbol)) return true;
	add_expected_diagnostic(std::format("symbol `{}`", ::get_variant_name(symbol)), reason);
	return false;
}

std::optional<std::string> Parser::expect_label(std::string_view reason) {
	EXPECT(consume_label, "label");
}

std::optional<std::string> Parser::expect_bare_unqualified_identifier(std::string_view reason) {
	EXPECT(consume_bare_unqualified_identifier, "identifier");
}

std::optional<Identifier> Parser::expect_unqualified_identifier(std::string_view reason) {
	EXPECT(consume_unqualified_identifier, "identifier");
}

std::optional<Identifier> Parser::expect_identifier(std::string_view reason) {
	EXPECT(consume_identifier, "(qualified) identifier");
}

std::optional<Type> Parser::expect_type_atom(std::string_view reason) {
	EXPECT(consume_type_atom, "type");
}

std::optional<Type> Parser::expect_type(std::string_view reason) {
	EXPECT(consume_type, "type");
}

std::optional<Expression> Parser::expect_expression_atom(std::string_view reason) {
	EXPECT(consume_expression_atom, "expression");
}

std::optional<Expression> Parser::expect_expression_function_call(std::string_view reason) {
	EXPECT(consume_expression_function_call, "expression");
}

std::optional<Expression> Parser::expect_expression_member_access(std::string_view reason) {
	EXPECT(consume_expression_member_access, "expression");
}

std::optional<Expression> Parser::expect_expression_unary_l1(std::string_view reason) {
	EXPECT(consume_expression_unary_l1, "expression");
}

std::optional<Expression> Parser::expect_expression_binop_l1(std::string_view reason) {
	EXPECT(consume_expression_binop_l1, "expression");
}

std::optional<Expression> Parser::expect_expression_binop_l2(std::string_view reason) {
	EXPECT(consume_expression_binop_l2, "expression");
}

std::optional<Expression> Parser::expect_expression_binop_l3(std::string_view reason) {
	EXPECT(consume_expression_binop_l3, "expression");
}

std::optional<Expression> Parser::expect_expression_binop_l4(std::string_view reason) {
	EXPECT(consume_expression_binop_l4, "expression");
}

std::optional<Expression> Parser::expect_expression(std::string_view reason) {
	EXPECT(consume_expression, "expression");
}

std::optional<GenericList> Parser::expect_generic_list(std::string_view reason) {
	EXPECT(consume_generic_list, "generic list");
}

std::optional<Statement> Parser::expect_statement(std::string_view reason) {
	EXPECT(consume_statement, "statement");
}

std::optional<Scope> Parser::expect_scope(std::string_view reason) {
	EXPECT(consume_scope, "scope");
}

char const* Parser::get_variant_name(Keyword keyword) {
	switch (keyword) {
	case Keyword::Def:    return "def";
	case Keyword::Import: return "import";
	case Keyword::Module: return "module";
	case Keyword::Export: return "export";
	case Keyword::Const:  return "const";
	case Keyword::Mut:    return "mut";
	case Keyword::Anon:   return "anon";
	case Keyword::Func:   return "func";
	case Keyword::Return: return "return";
	case Keyword::Goto:   return "goto";
	case Keyword::Branch: return "branch";
	case Keyword::If:     return "if";
	case Keyword::Else:   return "else";
	case Keyword::Struct: return "struct";
	}
}

void Parser::skip_semis() {
	tokens_.consume_while([](Token token) {
		return token.is_symbol() && token.get_symbol() == Token::Symbol::Semicolon;
	});
}

std::optional<Struct::Field> Parser::parse_struct_field() {
	auto name = SPANNED(consume_bare_unqualified_identifier);
	if (!name.has_value()) return {};
	if (!expect_symbol("expected `:` to specify field type", Token::Symbol::Colon)) return {};
	auto type = SPANNED_REASON(expect_type, "expected field type");
	if (!type.has_value()) return {};

	return Struct::Field {std::move(name.value()), std::move(type.value())};
}

std::optional<Struct> Parser::parse_struct() {
	if (!consume_keyword(Keyword::Struct)) return {};
	auto name = SPANNED_REASON(expect_identifier, "expected struct name");
	if (!name.has_value()) return {};
	std::optional<GenericDeclaration> generic_declaration = consume_generic_declaration();

	if (!expect_symbol("expected opening brace to begin struct body", Token::Symbol::LBrace)) return {};
	skip_semis();
	std::vector<Struct::Field> fields {};
	for (auto item = parse_struct_field(); item.has_value(); item = parse_struct_field()) {
		fields.push_back(std::move(item.value()));
		skip_semis();
	}
	skip_semis();
	expect_symbol("expected closing brace to end struct body", Token::Symbol::RBrace);

	return Struct {std::move(name.value()), std::move(generic_declaration), std::move(fields)};
}

std::optional<Function> Parser::parse_function() {
	if (!consume_keyword(Keyword::Func)) return {};
	auto name = SPANNED_REASON(expect_identifier, "expected function name");
	if (!name.has_value()) return {};
	std::optional<GenericDeclaration> generic_declaration = consume_generic_declaration();

	if (!expect_symbol("expected opening parenthesis to begin argument list", Token::Symbol::LParen)) return {};
	// parse args
	// TODO: do not allow any type in function signatures to be non-specific uint/int.
	std::vector<Function::Argument> arguments {};
	while (true) {
		auto argument_name = SPANNED(consume_unqualified_identifier);
		if (!argument_name.has_value()) break;
		// we know arguments may be anonymous or mutable
		bool anonymous = false, mutable_ = false;
		// store the span for diagnostics
		std::optional<Span> anon_span, mut_span;
		// if we get `anon` or `mut`, they might be qualifying the argument
		while ((argument_name.value().value.name() == "anon" || argument_name.value().value.name() == "mut")
		       && peek_unqualified_identifier()) {
			if (argument_name.value().value.name() == "anon") {
				if (anonymous)
					diagnostics_.push_back(
						Diagnostic::warning(
							"redundant `anon`",
							"argument was already marked as anonymous",
							{Diagnostic::Sample(
								context_,
								{Diagnostic::Sample::Label(
									 argument_name.value().span,
									 OutFmt::Color::Yellow
								 ),
					                         Diagnostic::Sample::Label(
									 anon_span.value(),
									 "first marked here",
									 OutFmt::Color::Cyan
								 )}
							)}
						)
					);
				anon_span = argument_name.value().span;
				anonymous = true;
			} else {
				if (mutable_)
					diagnostics_.push_back(
						Diagnostic::warning(
							"redundant `mut`",
							"argument was already marked as mutable",
							{Diagnostic::Sample(
								context_,
								{Diagnostic::Sample::Label(
									 argument_name.value().span,
									 OutFmt::Color::Yellow
								 ),
					                         Diagnostic::Sample::Label(
									 mut_span.value(),
									 "first marked here",
									 OutFmt::Color::Cyan
								 )}
							)}
						)
					);
				mut_span = argument_name.value().span;
				mutable_ = true;
			}
			argument_name = SPANNED(consume_unqualified_identifier);
		}
		if (!expect_symbol("expected `:` to specify argument type", Token::Symbol::Colon)) return {};
		auto argument_type = SPANNED_REASON(expect_type, "expected argument type");
		if (!argument_type.has_value()) return {};

		auto duplicate_argument = std::find_if(
			arguments.cbegin(),
			arguments.cend(),
			[&argument_name](Function::Argument const& argument) {
				return argument.name.value.name() == argument_name.value().value.name();
			}
		);

		if (duplicate_argument != arguments.cend()) {
			diagnostics_.push_back(
				Diagnostic::error(
					"duplicate argument name",
					"argument name used twice in function declaration",
					{Diagnostic::Sample(
						context_,
						{Diagnostic::Sample::Label(
							 argument_name.value().span,
							 OutFmt::Color::Red
						 ),
			                         Diagnostic::Sample::Label(
							 duplicate_argument->name.span,
							 "first used here",
							 OutFmt::Color::Cyan
						 )}
					)}
				)
			);
		} else {
			// we only push the argument if it's not duplicate to avoid confusing later stages
			arguments.emplace_back(
				argument_name.value(),
				std::move(argument_type.value()),
				anonymous,
				mutable_
			);
		}
		if (!consume_single_comma_or_more()) break;
	}
	if (!expect_symbol("expected closing parenthesis to end argument list", Token::Symbol::RParen)) return {};

	// default to void
	auto return_type
		= SPANNED(consume_type).value_or(Spanned {name.value().span, Type::make_atom(Type::Atom::make_void())});

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
			body.value().push_back(Spanned<Statement> {expression.value().span, std::move(statement)});
		}

		// we need to delimit the expression by a semicolon
		expect_semicolon("expected semicolon after function body shorthand");
	} else {
		// otherwise, we get a regular body
		body = consume_scope();

		// if we didn't get a body, we need a semicolon once more
		if (!body.has_value()) expect_semicolon("expected semicolon after function declaration without body");
	}

	return Function {
		name.value(),
		std::move(generic_declaration),
		std::move(arguments),
		std::move(return_type),
		std::move(body)
	};
}

std::optional<Alias> Parser::parse_alias() {
	if (!consume_keyword(Keyword::Def)) return {};
	std::optional<Spanned<Identifier>> name = SPANNED_REASON(expect_unqualified_identifier, "expected alias name");
	if (!name.has_value()) return {};
	std::optional<Spanned<Identifier>> value = SPANNED_REASON(expect_identifier, "expected alias value");
	if (!value.has_value()) return {};
	if (!value.value().value.absolute)
		diagnostics_.push_back(
			Diagnostic::error(
				"unsupported relative alias",
				"aliases must be absolute (for now)",
				{Diagnostic::Sample(context_, value.value().span)}
			)
		);
	expect_semicolon("expected semicolon after alias");
	return Alias {std::move(name.value()), std::move(value.value())};
}

std::optional<Import> Parser::parse_import() {
	if (!consume_keyword(Keyword::Import)) return {};
	std::optional<Spanned<Identifier>> name
		= SPANNED_REASON(expect_identifier, "expected name of the item to import");
	if (!name.has_value()) return {};
	if (name.value().value.absolute)
		diagnostics_.push_back(
			Diagnostic::warning(
				"redundant absolute qualified identifier marker",
				"imports are always absolute",
				{Diagnostic::Sample(
					context_,
					Span(name.value().span.start, name.value().span.start + 2)
				)}
			)
		);
	name.value().value.absolute = true;
	expect_semicolon("expected semicolon after alias");
	return Import {std::move(name.value())};
}

std::optional<Module> Parser::parse_module() {
	if (!consume_keyword(Keyword::Module)) return {};
	std::optional<Spanned<Identifier>> name = SPANNED_REASON(expect_identifier, "expected module name");
	if (!name.has_value()) return {};
	std::optional<Module::Body> body = parse_module_body();
	if (!body.has_value()) return {};
	return Module {name.value(), std::move(body.value())};
}

std::optional<Module::Item> Parser::parse_module_item() {
	std::vector<Tag>   tags {};
	std::optional<Tag> tag {};
	while ((tag = consume_tag()).has_value()) tags.push_back(std::move(tag.value()));

	// if we find export, we consume it
	bool                exported = consume_keyword(Keyword::Export);
	std::optional<Span> exported_span
		= exported ? std::optional {tokens_.at(tokens_.index() - 1).value().span()} : std::nullopt;

	std::optional<Module::Item> item;
	if (peek_keyword(Keyword::Module)) {
		item = parse_module().transform([tags = std::move(tags), exported](auto&& value) {
			return Module::Item {std::move(tags), exported, std::move(value)};
		});
	} else if (peek_keyword(Keyword::Func)) {
		item = parse_function().transform([tags = std::move(tags), exported](auto&& value) {
			return Module::Item {std::move(tags), exported, std::move(value)};
		});
	} else if (peek_keyword(Keyword::Def)) {
		item = parse_alias().transform([tags = std::move(tags), exported](auto&& value) {
			return Module::Item {std::move(tags), exported, std::move(value)};
		});
	} else if (peek_keyword(Keyword::Import)) {
		item = parse_import().transform([tags = std::move(tags), exported, this, exported_span](auto&& value) {
			if (exported)
				diagnostics_.push_back(
					Diagnostic::error(
						"imports cannot be exported",
						"imports merely mark names as available, so you might have meant to export an alias instead",
						{Diagnostic::Sample(context_, exported_span.value())}
					)
				);
			return Module::Item {std::move(tags), exported, std::move(value)};
		});
	} else if (peek_keyword(Keyword::Struct)) {
		item = parse_struct().transform([tags = std::move(tags), exported](auto&& value) {
			return Module::Item {std::move(tags), exported, std::move(value)};
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

Module Parser::parse_all(std::string name) {
	Module::Body body = parse_module_body(true).value_or(Module::Body {});
	return Module {
		Spanned<Identifier> {Span(0), Identifier(Spanned<std::string> {Span(0), name})},
		std::move(body)
	};
}

}  // namespace AST
