#include "ast/trait.hpp"

namespace AST {

std::ostream& operator<<(std::ostream& os, Trait::Constraint const& constraint) {
	os << constraint.name.value;
	return os;
}

std::ostream& operator<<(std::ostream& os, Trait const& trait) {
	os << "declare trait " << trait.name.value;
	if (trait.generic_declaration.has_value()) os << trait.generic_declaration.value();
	if (!trait.constraints.empty()) {
		os << ": ";
		size_t count = 0;
		for (Trait::Constraint const& constraint : trait.constraints) {
			os << constraint;
			if (++count < trait.constraints.size()) os << " + ";
		}
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

std::ostream& operator<<(std::ostream& os, TraitImplementation const& implementation) {
	os << "mark";
	if (implementation.generic_declaration.has_value()) os << implementation.generic_declaration.value();
	os << " type " << implementation.type.value << " as " << implementation.name.value;
	if (!implementation.methods.empty()) {
		os << ", providing implementations: {\n";
		os.iword(0)++;
		for (auto const& method : implementation.methods) {
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
