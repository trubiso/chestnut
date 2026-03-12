#include "ast/trait.hpp"

namespace AST {

std::ostream& operator<<(std::ostream& os, Trait::Constraint const& constraint) {
	os << constraint.name.value;
	if (constraint.generic_list.has_value()) os << constraint.generic_list.value();
	return os;
}

std::ostream& operator<<(std::ostream& os, Trait const& trait) {
	os << "declare trait " << trait.name.value;
	if (trait.generic_declaration.has_value()) os << trait.generic_declaration.value();
	os << ": ";
	size_t count = 0;
	for (Trait::Constraint const& constraint : trait.constraints) {
		os << constraint;
		if (++count < trait.constraints.size()) os << " + ";
	}
	if (!trait.methods.empty()) {
		os << " with methods: {\n";
		os.iword(0)++;
		for (auto const& method : trait.methods) {
			for (long i = 0; i < os.iword(0); ++i) os << "    ";
			os << method << '\n';
		}
		os.iword(0)--;
		for (long i = 0; i < os.iword(0); ++i) os << "    ";
		os << "}";
	}
	return os;
}

}  // namespace AST
