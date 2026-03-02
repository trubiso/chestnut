#include "trait.hpp"

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
	return os;
}

}  // namespace AST
