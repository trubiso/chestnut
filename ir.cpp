#include "ir.hpp"

#include <iostream>

namespace IR {

std::ostream& operator<<(std::ostream& os, Type::Atom const& atom) {
	switch (atom.kind()) {
	case Type::Atom::Kind::Integer: break;
	case Type::Atom::Kind::Float:   return os << "float" << (uint32_t) atom.get_float().width_value();
	case Type::Atom::Kind::Void:    return os << "void";
	case Type::Atom::Kind::Char:    return os << "char";
	case Type::Atom::Kind::Bool:    return os << "bool";
	case Type::Atom::Kind::Error:   return os << "(error)";
	}

	// we know it's an integer now
	Type::Atom::Integer int_ = atom.get_integer();
	os << (int_.is_signed() ? "int" : "uint");
	Type::Atom::Integer::WidthType width_type = int_.width_type();

	switch (width_type) {
	case Type::Atom::Integer::WidthType::Fixed: return os << int_.bit_width().value();
	case Type::Atom::Integer::WidthType::Ptr:   return os << "ptr";
	case Type::Atom::Integer::WidthType::Size:  return os << "size";
	}
	[[assume(false)]];
}

std::ostream& operator<<(std::ostream& os, Type const& type) {
	switch (type.kind()) {
	case Type::Kind::Atom: return os << type.get_atom();
	}
	[[assume(false)]];
}

std::ostream& operator<<(std::ostream& os, BuiltInFunction const& built_in_function) {
	os << '%';
	switch (built_in_function) {
	case BuiltInFunction::AddUIntegers:      return os << "AddUIntegers";
	case BuiltInFunction::AddSIntegers:      return os << "AddSIntegers";
	case BuiltInFunction::AddFloats:         return os << "AddFloats";
	case BuiltInFunction::SubtractUIntegers: return os << "SubtractUIntegers";
	case BuiltInFunction::SubtractSIntegers: return os << "SubtractSIntegers";
	case BuiltInFunction::SubtractFloats:    return os << "SubtractFloats";
	case BuiltInFunction::MultiplyUIntegers: return os << "MultiplyUIntegers";
	case BuiltInFunction::MultiplySIntegers: return os << "MultiplySIntegers";
	case BuiltInFunction::MultiplyFloats:    return os << "MultiplyFloats";
	case BuiltInFunction::DivideUIntegers:   return os << "DivideUIntegers";
	case BuiltInFunction::DivideSIntegers:   return os << "DivideSIntegers";
	case BuiltInFunction::DivideFloats:      return os << "DivideFloats";
	case BuiltInFunction::NegateSInteger:    return os << "NegateSInteger";
	case BuiltInFunction::NegateFloat:       return os << "NegateFloat";
	}
}

std::ostream& operator<<(std::ostream& os, Expression::Atom::Literal const& literal) {
	switch (literal.kind) {
	case Expression::Atom::Literal::Kind::Number: os << "(number) "; break;
	case Expression::Atom::Literal::Kind::String: os << "(string) "; break;
	case Expression::Atom::Literal::Kind::Char:   os << "(char) "; break;
	}
	return os << literal.literal;
}

std::ostream& operator<<(std::ostream& os, Expression::Atom const& atom) {
	switch (atom.kind()) {
	case Expression::Atom::Kind::Identifier: return os << '@' << atom.get_identifier();
	case Expression::Atom::Kind::Literal:    return os << atom.get_literal();
	}
	[[assume(false)]];
}

std::ostream& operator<<(std::ostream& os, Expression::FunctionCall const& call) {
	os << "(call ";
	if (std::holds_alternative<AST::SymbolID>(call.callee)) os << '@' << std::get<AST::SymbolID>(call.callee);
	else os << std::get<BuiltInFunction>(call.callee);
	if (!call.arguments.empty()) {
		os << " w/ args: (";
		for (size_t i = 0; i < call.arguments.size(); ++i) {
			os << call.arguments[i].value;
			if (i + 1 < call.arguments.size()) os << ", ";
		}
		os << ')';
	}
	return os << ')';
}

std::ostream& operator<<(std::ostream& os, Expression const& expression) {
	switch (expression.kind()) {
	case Expression::Kind::Atom:         return os << expression.get_atom();
	case Expression::Kind::FunctionCall: return os << expression.get_function_call();
	}
	[[assume(false)]];
}

std::ostream& operator<<(std::ostream& os, Statement::Declare const& declare) {
	os << "[declare stmt: ";
	os << (declare.mutable_.value ? "mut" : "const") << " @" << declare.name.value;
	os << ": " << declare.type;
	if (declare.value.has_value()) os << " = " << declare.value.value().value;
	return os << ";]";
}

std::ostream& operator<<(std::ostream& os, Statement::Set const& set) {
	os << "[set stmt: ";
	os << '@' << set.name.value << " = " << set.value.value;
	return os << ";]";
}

std::ostream& operator<<(std::ostream& os, Statement::Return const& return_) {
	os << "[return stmt: ";
	if (return_.value.has_value()) os << return_.value.value().value;
	else os << "(no value)";
	return os << ";]";
}

std::ostream& operator<<(std::ostream& os, Statement const& statement) {
	for (long i = 0; i < os.iword(0); ++i) os << "    ";
	switch (statement.kind()) {
	case Statement::Kind::Declare: return os << statement.get_declare();
	case Statement::Kind::Set:     return os << statement.get_set();
	case Statement::Kind::Call:    return os << "[call stmt: " << statement.get_call() << ";]";
	case Statement::Kind::Return:  return os << statement.get_return();
	}
}

std::ostream& operator<<(std::ostream& os, Function const& function) {
	os << "declare function @" << function.name.value << " w/ args (";
	size_t count = 0;
	for (auto const& arg : function.arguments) {
		os << '@' << arg.name.value << ": " << arg.type.value;
		if (++count < function.arguments.size()) os << ", ";
	}
	os << ")";
	if (function.body.has_value()) {
		Scope const& scope = function.body.value();
		os << ": ";
		if (scope.empty()) return os << "(empty body)";
		os << "{\n";
		os.iword(0)++;
		for (auto const& stmt : scope) { os << stmt.value << '\n'; }
		os.iword(0)--;
		for (long i = 0; i < os.iword(0); ++i) os << "    ";
		os << "}";
	}
	return os;
}

}  // namespace IR
