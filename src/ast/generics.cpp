#include "generics.hpp"

namespace AST {

std::ostream& operator<<(std::ostream& os, GenericDeclaration const& generic_declaration) {
	os << '<';
	size_t count = 0;
	for (auto const& generic : generic_declaration.generics) {
		if (generic.anonymous) os << "anon ";
		os << generic.name.value.name();
		if (++count < generic_declaration.generics.size()) os << ", ";
	}
	return os << '>';
}

std::ostream& operator<<(std::ostream& os, GenericList const& generic_list) {
	os << '<';
	for (size_t i = 0; i < generic_list.ordered.size(); ++i) {
		os << generic_list.ordered[i].value;
		if (!generic_list.labeled.empty() || i + 1 < generic_list.ordered.size()) os << ", ";
	}
	for (size_t i = 0; i < generic_list.labeled.size(); ++i) {
		os << std::get<0>(generic_list.labeled[i]).value << ": " << std::get<1>(generic_list.labeled[i]).value;
		if (i + 1 < generic_list.labeled.size()) os << ", ";
	}
	return os << '>';
}

}  // namespace AST
