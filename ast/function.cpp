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

}  // namespace AST
