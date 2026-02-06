#include "statement.hpp"

namespace AST {

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
	for (long i = 0; i < os.iword(0); ++i) os << "    ";
	switch (statement.kind()) {
	case Statement::Kind::Declare:    return os << statement.get_declare();
	case Statement::Kind::Set:        return os << statement.get_set();
	case Statement::Kind::Expression: return os << "[expr stmt: " << statement.get_expression() << ";]";
	case Statement::Kind::Return:     return os << statement.get_return();
	case Statement::Kind::Scope:      break;
	case Statement::Kind::Label:      return os << "[label '" << statement.get_label().name << ":]";
	case Statement::Kind::Goto:       return os << "[goto '" << statement.get_goto().destination << ";]";
	}

	Scope const& scope = statement.get_scope();
	os << "[scope stmt";
	if (scope.empty()) return os << " (empty)]";
	else os << ":\n";
	os.iword(0)++;
	for (size_t i = 0; i < scope.size(); ++i) { os << scope[i].value << '\n'; }
	os.iword(0)--;
	for (long i = 0; i < os.iword(0); ++i) os << "    ";
	return os << ']';
}

}  // namespace AST
