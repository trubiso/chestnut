#include "resolver.hpp"

#include <iostream>

std::vector<IR::Module> Resolver::resolve() {
	identify_built_in_operators();
	populate_module_table();
	identify_module_items();
	resolve_identifiers();
	infer_types();

	auto lowered = lower();
	for (IR::Module const& module : lowered) {
		print(module);
		std::cout << std::endl;
	}
	return lowered;
}

void Resolver::print(IR::Module const& module) const {
	std::cout << "declare module @" << module.name.value << ": ";
	if (module.items.empty()) {
		std::cout << "(empty module)";
		return;
	}
	std::cout << "{\n";
	std::cout.iword(0)++;
	for (auto item : module.items) {
		for (long i = 0; i < std::cout.iword(0); ++i) std::cout << "    ";
		if (std::holds_alternative<IR::Module>(symbol_pool_.at(item).item))
			print(std::get<IR::Module>(symbol_pool_.at(item).item));
		else if (std::holds_alternative<IR::Function>(symbol_pool_.at(item).item))
			std::cout << std::get<IR::Function>(symbol_pool_.at(item).item);
		std::cout << '\n';
	}
	std::cout.iword(0)--;
	for (long i = 0; i < std::cout.iword(0); ++i) std::cout << "    ";
	std::cout << "}";
}

std::vector<IR::Symbol> Resolver::export_symbols() const {
	std::vector<IR::Symbol> symbols {};
	symbols.reserve(symbol_pool_.size());
	for (Symbol const& symbol : symbol_pool_)
		symbols.push_back(
			IR::Symbol {
				symbol.id,
				symbol.file_id,
				symbol.span,
				symbol.name,
				std::holds_alternative<IR::Module>(symbol.item)
					? decltype(IR::Symbol::item) {std::get<IR::Module>(symbol.item)}
				: std::holds_alternative<IR::Function>(symbol.item)
					? decltype(IR::Symbol::item) {std::get<IR::Function>(symbol.item)}
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
