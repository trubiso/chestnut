#include "resolver.hpp"

#include <iostream>

void Resolver::resolve() {
	populate_module_table();
	identify_module_items();
	resolve_identifiers();
	infer_types();
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

FileContext Resolver::get_context(FileContext::ID file_id) {
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
