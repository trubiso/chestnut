#include "token.hpp"

size_t Token::size() const {
	switch ((Token::Kind) value.index()) {
	case Kind::Identifier:    return std::get<(size_t) Kind::Identifier>(value).size();
	case Kind::NumberLiteral: return std::get<(size_t) Kind::NumberLiteral>(value).size();
	case Kind::StringLiteral: return std::get<(size_t) Kind::StringLiteral>(value).size() + 2;
	case Kind::CharLiteral:   return 3;  // FIXME: this could actually be more if the character is escaped
	case Kind::Symbol:
		switch (std::get<(size_t) Kind::Symbol>(value)) {
		case Symbol::Plus:
		case Symbol::Minus:
		case Symbol::Star:
		case Symbol::Div:
		case Symbol::Amp:
		case Symbol::Bar:
		case Symbol::Xor:
		case Symbol::Tilde:
		case Symbol::Eq:
		case Symbol::Lt:
		case Symbol::Gt:
		case Symbol::Bang:
		case Symbol::Maybe:
		case Symbol::Percent:
		case Symbol::At:
		case Symbol::Dot:
		case Symbol::LParen:
		case Symbol::RParen:
		case Symbol::LBracket:
		case Symbol::RBracket:
		case Symbol::LBrace:
		case Symbol::RBrace:
		case Symbol::Comma:
		case Symbol::Colon:
		case Symbol::Semicolon:  return 1;
		case Symbol::EqEq:
		case Symbol::Le:
		case Symbol::Ge:
		case Symbol::LtGt:
		case Symbol::Ne:
		case Symbol::MaybeMaybe:
		case Symbol::MaybeDot:
		case Symbol::DotDot:
		case Symbol::LeftArrow:
		case Symbol::ColonColon:
		case Symbol::Arrow:
		case Symbol::FatArrow:   return 2;
		case Symbol::DotDotLt:
		case Symbol::DotDotEq:
		case Symbol::DotDotDot:  return 3;
		default:                 [[assume(false)]];
		}
	}
	[[assume(false)]];
	return 0;
}

std::ostream& operator<<(std::ostream& os, Token const& token) {
#define VISIT(kind, l, r)                                                               \
	case (size_t) kind: return os << l << std::get<(size_t) kind>(token.value) << r
	switch (token.value.index()) {
		VISIT(Token::Kind::Identifier, "[id ", "]");
		VISIT(Token::Kind::NumberLiteral, "[num ", "]");
		VISIT(Token::Kind::StringLiteral, "[string ", "]");
		VISIT(Token::Kind::CharLiteral, "[char ", "]");
		VISIT(Token::Kind::Symbol, "[symbol ", "]");
	default: [[assume(false)]]; return os;
	}
#undef VISIT
}

char const* get_variant_name(Token::Symbol symbol) {
	switch (symbol) {
	case Token::Symbol::Plus:                  return "+";
	case Token::Symbol::Minus:                 return "-";
	case Token::Symbol::Star:                  return "*";
	case Token::Symbol::Div:                   return "/";
	case Token::Symbol::Amp:                   return "&";
	case Token::Symbol::Bar:                   return "|";
	case Token::Symbol::Xor:                   return "^";
	case Token::Symbol::Tilde:                 return "~";
	case Token::Symbol::Eq:                    return "=";
	case Token::Symbol::EqEq:                  return "==";
	case Token::Symbol::Lt:                    return "<";
	case Token::Symbol::Le:                    return "<=";
	case Token::Symbol::Gt:                    return ">";
	case Token::Symbol::Ge:                    return ">=";
	case Token::Symbol::LtGt:                  return "<>";
	case Token::Symbol::Ne:                    return "!=";
	case Token::Symbol::Bang:                  return "!";
	case Token::Symbol::Maybe:                 return "?";
	case Token::Symbol::MaybeMaybe:            return "??";
	case Token::Symbol::MaybeDot:              return "??";
	case Token::Symbol::Percent:               return "%";
	case Token::Symbol::At:                    return "@";
	case Token::Symbol::Dot:                   return ".";
	case Token::Symbol::DotDot:                return "..";
	case Token::Symbol::DotDotLt:              return "..<";
	case Token::Symbol::DotDotEq:              return "..=";
	case Token::Symbol::LeftArrow:             return "<-";
	case Token::Symbol::LParen:                return "(";
	case Token::Symbol::RParen:                return ")";
	case Token::Symbol::LBracket:              return "[";
	case Token::Symbol::RBracket:              return "]";
	case Token::Symbol::LBrace:                return "{";
	case Token::Symbol::RBrace:                return "}";
	case Token::Symbol::Comma:                 return ",";
	case Token::Symbol::Colon:                 return ":";
	case Token::Symbol::ColonColon:            return "::";
	case Token::Symbol::Semicolon:             return ";";
	case Token::Symbol::DotDotDot:             return "...";
	case Token::Symbol::Arrow:                 return "->";
	case Token::Symbol::FatArrow:              return "=>";
	case Token::Symbol::CommentStart:          return "//";
	case Token::Symbol::CommentMultilineStart: return "/*";
	}
	[[assume(false)]];
}

std::ostream& operator<<(std::ostream& os, Token::Symbol const& symbol) {
	return os << get_variant_name(symbol);
}
