#include "expression.hpp"

namespace AST {

bool Expression::Atom::NumberLiteral::is_float() const {
	// FIXME: this will break as soon as we add different float types
	return literal.contains('.');
}

bool Expression::can_be_lhs() const {
	// TODO: change this once deref, member access exist

	if (kind() != Kind::Atom) return false;
	return get_atom().kind() == Atom::Kind::Identifier;
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
	if (literal.suffix.has_value()) {
		return os << '[' << literal.literal << " w/ suffix " << literal.suffix.value() << ']';
	} else {
		return os << literal.literal;
	}
}

std::ostream& operator<<(std::ostream& os, Expression::Atom const& atom) {
	switch (atom.kind()) {
	case Expression::Atom::Kind::Identifier:    return os << atom.get_identifier();
	case Expression::Atom::Kind::NumberLiteral: return os << atom.get_number_literal();
	case Expression::Atom::Kind::StringLiteral: return os << atom.get_string_literal();
	case Expression::Atom::Kind::CharLiteral:   return os << atom.get_char_literal();
	case Expression::Atom::Kind::BoolLiteral:
		return os << '[' << (atom.get_bool_literal().value ? "true" : "false") << ']';
	case Expression::Atom::Kind::Expression: return os << '(' << *atom.get_expression() << ')';
	}
	[[assume(false)]];
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

std::ostream& operator<<(std::ostream& os, Expression::If const& if_) {
	return os
	    << "(if ("
	    << if_.condition->value
	    << ") "
	    << if_.true_->value
	    << " else "
	    << if_.false_->value
	    << ")";
}

std::ostream& operator<<(std::ostream& os, Expression const& expression) {
	switch (expression.kind()) {
	case Expression::Kind::Atom:            return os << expression.get_atom();
	case Expression::Kind::UnaryOperation:  return os << expression.get_unary_operation();
	case Expression::Kind::BinaryOperation: return os << expression.get_binary_operation();
	case Expression::Kind::FunctionCall:    return os << expression.get_function_call();
	case Expression::Kind::If:              return os << expression.get_if();
	}
	[[assume(false)]];
}

}  // namespace AST
