#include "module.hpp"

namespace AST {

std::ostream& operator<<(std::ostream& os, Import const& import) {
	return os << "import " << import.name.value;
}

std::ostream& operator<<(std::ostream& os, Module::InnerItem const& item) {
	if (std::holds_alternative<Function>(item))
		return os << std::get<Function>(item);
	else if (std::holds_alternative<Module>(item))
		return os << std::get<Module>(item);
	else if (std::holds_alternative<Import>(item))
		return os << std::get<Import>(item);
}

std::ostream& operator<<(std::ostream& os, Module::Item const& item) {
	for (auto const& tag : std::get<0>(item)) os << tag << " ";
	if (std::get<1>(item)) os << "(exported declaration) ";
	return os << std::get<Module::InnerItem>(item);
}

std::ostream& operator<<(std::ostream& os, Module const& module) {
	// FIXME: take indentation into account somehow
	os << "declare module " << module.name.value << ": ";
	if (module.body.items.empty()) return os << "(empty module)";
	os << "{\n";
	for (auto const& item : module.body.items) { os << '\t' << item.value << '\n'; }
	return os << "}";
}

}  // namespace AST
