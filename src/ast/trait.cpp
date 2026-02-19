#include "trait.hpp"

namespace AST {

std::ostream& operator<<(std::ostream& os, Trait::Named const& named) {
	return os << named.name.value;
}

std::ostream& operator<<(std::ostream& os, Trait::Has const& has) {
	os << "has " << has.name.value;
	if (has.generic_declaration.has_value()) os << " w/ generics " << has.generic_declaration.value();
	os << " w/ args (";
	size_t count = 0;
	for (auto const& arg : has.arguments) {
		if (arg.anonymous) os << "(anonymous \"" << arg.name.value << "\")";
		else os << arg.name.value;
		os << ": " << arg.type.value;
		if (++count < has.arguments.size()) os << ", ";
	}
	return os << ") -> " << has.return_type.value;
}

std::ostream& operator<<(std::ostream& os, Trait::Constraint const& constraint) {
	if (std::holds_alternative<Trait::Named>(constraint)) return os << std::get<Trait::Named>(constraint);
	else if (std::holds_alternative<Trait::Has>(constraint)) return os << std::get<Trait::Has>(constraint);
	[[assume(false)]];
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
