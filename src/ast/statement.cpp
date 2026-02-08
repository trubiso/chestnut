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

std::ostream& operator<<(std::ostream& os, Statement::Label const& label) {
	os << "[label '" << label.name;
	if (label.id.has_value()) os << " ('@" << label.id.value() << ")";
	return os << ":]";
}

std::ostream& operator<<(std::ostream& os, Statement::Goto const& goto_) {
	os << "[goto '" << goto_.destination;
	if (goto_.destination_id.has_value()) os << " ('@" << goto_.destination_id.value() << ")";
	return os << ";]";
}

std::ostream& operator<<(std::ostream& os, Statement::Branch const& branch) {
	os << "[branch (" << branch.condition.value << ") '" << branch.true_.value.destination;
	if (branch.true_.value.destination_id.has_value())
		os << " ('@" << branch.true_.value.destination_id.value() << ")";
	if (branch.false_.has_value()) {
		os << " '" << branch.false_.value().value.destination;
		if (branch.false_.value().value.destination_id.has_value())
			os << " ('@" << branch.false_.value().value.destination_id.value() << ")";
	}
	return os << ";]";
}

std::ostream& operator<<(std::ostream& os, Statement::If const& if_) {
	os << "[if (" << if_.condition.value << ") then:\n";
	os.iword(0)++;
	for (size_t i = 0; i < if_.true_.value.size(); ++i) { os << if_.true_.value[i].value << '\n'; }
	os.iword(0)--;
	for (long i = 0; i < os.iword(0); ++i) os << "    ";
	if (if_.false_.has_value()) {
		os << "else:\n";
		os.iword(0)++;
		for (size_t i = 0; i < if_.false_.value().value.size(); ++i) {
			os << if_.false_.value().value[i].value << '\n';
		}
		os.iword(0)--;
		for (long i = 0; i < os.iword(0); ++i) os << "    ";
	}
	return os << ']';
}

std::ostream& operator<<(std::ostream& os, Statement const& statement) {
	for (long i = 0; i < os.iword(0); ++i) os << "    ";
	switch (statement.kind()) {
	case Statement::Kind::Declare:    return os << statement.get_declare();
	case Statement::Kind::Set:        return os << statement.get_set();
	case Statement::Kind::Expression: return os << "[expr stmt: " << statement.get_expression() << ";]";
	case Statement::Kind::Return:     return os << statement.get_return();
	case Statement::Kind::Scope:      break;
	case Statement::Kind::Label:      return os << statement.get_label();
	case Statement::Kind::Goto:       return os << statement.get_goto();
	case Statement::Kind::Branch:     return os << statement.get_branch();
	case Statement::Kind::If:         return os << statement.get_if();
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
