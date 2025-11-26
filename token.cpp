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

std::ostream& operator<<(std::ostream& os, Token::Symbol const& symbol) {
	switch (symbol) {
	case Token::Symbol::Plus:                  return os << "+";
	case Token::Symbol::Minus:                 return os << "-";
	case Token::Symbol::Star:                  return os << "*";
	case Token::Symbol::Div:                   return os << "/";
	case Token::Symbol::Amp:                   return os << "&";
	case Token::Symbol::Bar:                   return os << "|";
	case Token::Symbol::Xor:                   return os << "^";
	case Token::Symbol::Tilde:                 return os << "~";
	case Token::Symbol::Eq:                    return os << "=";
	case Token::Symbol::EqEq:                  return os << "==";
	case Token::Symbol::Lt:                    return os << "<";
	case Token::Symbol::Le:                    return os << "<=";
	case Token::Symbol::Gt:                    return os << ">";
	case Token::Symbol::Ge:                    return os << ">=";
	case Token::Symbol::LtGt:                  return os << "<>";
	case Token::Symbol::Ne:                    return os << "!=";
	case Token::Symbol::Bang:                  return os << "!";
	case Token::Symbol::Maybe:                 return os << "?";
	case Token::Symbol::MaybeMaybe:            return os << "??";
	case Token::Symbol::MaybeDot:              return os << "??";
	case Token::Symbol::Percent:               return os << "%";
	case Token::Symbol::At:                    return os << "@";
	case Token::Symbol::Dot:                   return os << ".";
	case Token::Symbol::DotDot:                return os << "..";
	case Token::Symbol::DotDotLt:              return os << "..<";
	case Token::Symbol::DotDotEq:              return os << "..=";
	case Token::Symbol::LeftArrow:             return os << "<-";
	case Token::Symbol::LParen:                return os << "(";
	case Token::Symbol::RParen:                return os << ")";
	case Token::Symbol::LBracket:              return os << "[";
	case Token::Symbol::RBracket:              return os << "]";
	case Token::Symbol::LBrace:                return os << "{";
	case Token::Symbol::RBrace:                return os << "}";
	case Token::Symbol::Comma:                 return os << ",";
	case Token::Symbol::Colon:                 return os << ":";
	case Token::Symbol::ColonColon:            return os << "::";
	case Token::Symbol::Semicolon:             return os << ";";
	case Token::Symbol::DotDotDot:             return os << "...";
	case Token::Symbol::Arrow:                 return os << "->";
	case Token::Symbol::FatArrow:              return os << "=>";
	case Token::Symbol::CommentStart:          return os << "//";
	case Token::Symbol::CommentMultilineStart: return os << "/*";
	}
	[[assume(false)]];
}
