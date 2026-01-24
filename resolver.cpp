#include "resolver.hpp"

#include <format>
#include <iostream>
#include <sstream>

void Resolver::resolve() {
	populate_module_table();
	identify_module_items();
	resolve_imports();
	resolve_identifiers();
}

uint32_t Resolver::next() {
	return counter_++;
}

void Resolver::populate_module_table() {
	for (ParsedFile& file : parsed_files) {
		std::string name = std::string(file.module.name.value.name);
		// TODO: do something else about this
		if (module_table_.contains(name)) std::cout << "this should never happen! clashing names! SOS!";
		else module_table_.emplace(name, &file.module);
	}
}

void Resolver::identify(AST::Identifier& identifier) {
	identifier.id = next();
}

void Resolver::identify(AST::Module& module) {
	identify(module.name.value);
	for (Spanned<AST::Module::Item>& item : module.body.items) {
		auto& value = std::get<std::variant<AST::Function, AST::Module, AST::Import>>(item.value);
		if (std::holds_alternative<AST::Function>(value)) identify(std::get<AST::Function>(value));
		else if (std::holds_alternative<AST::Module>(value)) identify(std::get<AST::Module>(value));
	}
}

void Resolver::identify(AST::Function& function) {
	identify(function.name.value);
}

void Resolver::identify_module_items() {
	for (ParsedFile& file : parsed_files) { identify(file.module); }
}

void Resolver::resolve(AST::Module& module) {
	for (Spanned<AST::Module::Item>& item : module.body.items) {
		auto& value = std::get<std::variant<AST::Function, AST::Module, AST::Import>>(item.value);
		if (std::holds_alternative<AST::Import>(value)) {
			auto& import = std::get<AST::Import>(value);
			// now we found an import, time to see if we can find what it refers to
			if (import.name.value.absolute) {
				// if it's absolute, we just need to start in our module table
				if (!module_table_.contains(std::string(import.name.value.path[0].value))) {
					std::stringstream title_stream {}, subtitle_stream {};
					title_stream << "unknown identifier '";
					title_stream << import.name.value.path[0].value;
					title_stream << '\'';
					subtitle_stream
						<< "could not find the provided identifier in the global scope (available: ";
					size_t count = 0;
					for (auto const& v : module_table_) {
						subtitle_stream << '\'' << v.first << '\'';
						if (++count < module_table_.size()) subtitle_stream << ", ";
					}
					subtitle_stream << ')';
					std::string title    = title_stream.str();
					std::string subtitle = subtitle_stream.str();
					diagnostics_.push_back(
						Diagnostic::error(
							std::move(title),
							std::move(subtitle),
							{Diagnostic::Sample(import.name.value.path[0].span)}
						)
					);
					continue;
				}
				AST::Module* current_module;
				for (size_t i = 0; i < import.name.value.path.size(); ++i) {
					Spanned<std::string> const& fragment = import.name.value.path[i];
				}
			}
		}
		if (std::holds_alternative<AST::Function>(value)) identify(std::get<AST::Function>(value));
		else if (std::holds_alternative<AST::Module>(value)) identify(std::get<AST::Module>(value));
	}
}

void Resolver::resolve_imports() {
	// we will resolve the QualifiedIdentifier in each of the imports
	for (ParsedFile& file : parsed_files) { resolve(file.module); }
}

void Resolver::resolve_identifiers() {}
