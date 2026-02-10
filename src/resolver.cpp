#include "resolver.hpp"

#include <iostream>

std::vector<IR::Module> Resolver::resolve() {
	identify_built_in_operators();
	desugar_control_flow();
	identify_labels();
	populate_module_table();
	identify_module_items();
	resolve_identifiers();
	infer_types();

	auto lowered = lower();
	for (IR::Module const& module : lowered) { print(std::cout, module) << std::endl; }
	return lowered;
}

std::ostream& Resolver::print(std::ostream& os, IR::Module const& module) const {
	os << "declare module @" << module.name.value << ": ";
	if (module.items.empty()) { return os << "(empty module)"; }
	os << "{\n";
	os.iword(0)++;
	for (auto item : module.items) {
		for (long i = 0; i < os.iword(0); ++i) os << "    ";
		if (std::holds_alternative<IR::Module>(symbol_pool_.at(item).item))
			print(os, std::get<IR::Module>(symbol_pool_.at(item).item));
		else if (std::holds_alternative<IR::Function>(symbol_pool_.at(item).item))
			os << std::get<IR::Function>(symbol_pool_.at(item).item);
		os << '\n';
	}
	os.iword(0)--;
	for (long i = 0; i < os.iword(0); ++i) os << "    ";
	return os << "}";
}

std::vector<IR::Symbol> Resolver::export_symbols() {
	std::vector<IR::Symbol> symbols {};
	symbols.reserve(symbol_pool_.size());
	for (Symbol& symbol : symbol_pool_)
		symbols.push_back(
			IR::Symbol {
				symbol.id,
				symbol.file_id,
				symbol.span,
				symbol.name,
				std::holds_alternative<IR::Module>(symbol.item)
					? decltype(IR::Symbol::item) {std::get<IR::Module>(symbol.item)}
				: std::holds_alternative<IR::Function>(symbol.item)
					? decltype(IR::Symbol::item) {std::move(std::get<IR::Function>(symbol.item))}
				: std::holds_alternative<IR::BuiltInFunction>(symbol.item)
					? decltype(IR::Symbol::item) {std::get<IR::BuiltInFunction>(symbol.item)}
					: std::monostate {},
				symbol.mutable_
			}
		);
	return symbols;
}

void Resolver::dump() const {
	std::cout << symbol_pool_.size() << " symbols, " << type_pool_.size() << " types\n";
	for (Symbol const& symbol : symbol_pool_) {
		std::cout << '@' << symbol.id << " (file #" << symbol.file_id << "): " << symbol.name;
		if (symbol.mutable_) std::cout << " (mutable)";
		std::cout << "\n    ";
		debug_print_type(symbol.type);
		std::cout << std::endl;
	}
}

FileContext Resolver::get_context(FileContext::ID file_id) const {
	// TODO: don't do this
	if (file_id == 42) file_id = 0;
	return FileContext {
		parsed_files.at(file_id).name,
		file_id,
		parsed_files.at(file_id).loc,
		parsed_files.at(file_id).source,
	};
}

void Resolver::populate_module_table() {
	for (ParsedFile& file : parsed_files) {
		std::string name = file.module.name.value.name();
		// TODO: do something else about this
		if (module_table_.contains(name)) std::cout << "this should never happen! clashing names! SOS!";
		else module_table_.emplace(name, &file.module);
	}
}
