#include "function.hpp"

namespace AST {

std::ostream& operator<<(std::ostream& os, Function const& function) {
	os << "declare function " << function.name.value << " w/ args (";
	size_t count = 0;
	for (auto const& arg : function.arguments) {
		os << arg.name.value << ": " << arg.type.value;
		if (++count < function.arguments.size()) os << ", ";
	}
	os << ")";
	if (function.body.has_value()) {
		// FIXME: take indentation into account somehow
		Scope const& scope = function.body.value();
		os << ": ";
		if (scope.empty()) return os << "(empty body)";
		os << "{\n";
		for (auto const& stmt : scope) { os << '\t' << stmt.value << '\n'; }
		os << "}";
	}
	return os;
}

}  // namespace AST
