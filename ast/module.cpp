#include "module.hpp"

namespace AST {

std::ostream& operator<<(std::ostream& os, Import const& import) {
	return os << "import " << import.name.value;
}

std::ostream& operator<<(std::ostream& os, Module::InnerItem const& item) {
	if (std::holds_alternative<Function>(item)) return os << std::get<Function>(item);
	else if (std::holds_alternative<Module>(item)) return os << std::get<Module>(item);
	else if (std::holds_alternative<Import>(item)) return os << std::get<Import>(item);
}

std::ostream& operator<<(std::ostream& os, Module::Item const& item) {
	for (long i = 0; i < os.iword(0); ++i) os << "    ";
	for (auto const& tag : std::get<0>(item)) os << tag << " ";
	if (std::get<1>(item)) os << "(exported declaration) ";
	return os << std::get<Module::InnerItem>(item);
}

std::ostream& operator<<(std::ostream& os, Module const& module) {
	os << "declare module " << module.name.value << ": ";
	if (module.body.items.empty()) return os << "(empty module)";
	os << "{\n";
	os.iword(0)++;
	for (auto const& item : module.body.items) { os << item.value << '\n'; }
	os.iword(0)--;
	for (long i = 0; i < os.iword(0); ++i) os << "    ";
	return os << "}";
}

std::string const& Module::get_name(InnerItem const& inner_item) {
	if (std::holds_alternative<AST::Function>(inner_item))
		return std::get<AST::Function>(inner_item).name.value.name;
	else if (std::holds_alternative<AST::Module>(inner_item))
		return std::get<AST::Module>(inner_item).name.value.name;
	else if (std::holds_alternative<AST::Import>(inner_item))
		return std::get<AST::Import>(inner_item).name.value.last_fragment().value;
}

}  // namespace AST
