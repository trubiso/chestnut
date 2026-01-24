#include "resolver.hpp"

#include "ast/identifier.hpp"
#include "parser.hpp"

#include <cstdlib>
#include <iostream>
#include <sstream>

void Resolver::resolve() {
	populate_module_table();
	identify_module_items();

	populate_unresolved_imports();
	traverse_unresolved_imports();

	resolve_identifiers();
}

uint32_t Resolver::next() {
	return counter_++;
}

void Resolver::populate_module_table() {
	for (ParsedFile& file : parsed_files) {
		std::string name = file.module.name.value.name;
		// TODO: do something else about this
		if (module_table_.contains(name)) std::cout << "this should never happen! clashing names! SOS!";
		else module_table_.emplace(name, &file.module);
	}
}

void Resolver::identify(AST::Identifier& identifier) {
	identifier.id = next();
}

void Resolver::identify(AST::Module& module) {
	// TODO: ensure there are no duplicated item names (including imports)
	identify(module.name.value);
	for (Spanned<AST::Module::Item>& item : module.body.items) {
		auto& value = std::get<AST::Module::InnerItem>(item.value);
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

void Resolver::traverse_unresolved_imports() {
	for (auto& unresolved_import : unresolved_imports_) {
		auto& import = *unresolved_import.import;
		// now we found an import, time to see if we can find what it refers to
		if (import.name.value.absolute) {
			// if it's absolute, we just need to start in our module table
			if (!module_table_.contains(import.name.value.path[0].value)) {
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
			AST::Module const* root_module = module_table_.at(import.name.value.path[0].value);
			std::optional<AST::Function const*> pointed_item {};

			bool found = false;
			for (size_t i = 1; i < import.name.value.path.size(); ++i) {
				Spanned<std::string> const& fragment = import.name.value.path[i];
				if (pointed_item.has_value()) {
					// for now, this can only mean it is a function, which does not have subitems
					std::cout << "tried to access a function's subitems !" << std::endl;
					break;
				}
				found = false;
				for (auto const& item : root_module->body.items) {
					auto const& value = std::get<AST::Module::InnerItem>(item.value);
					// TODO: make a method for this name getting thing
					std::string name;
					if (std::holds_alternative<AST::Function>(value)) {
						name = std::get<AST::Function>(value).name.value.name;
					} else if (std::holds_alternative<AST::Module>(value)) {
						name = std::get<AST::Module>(value).name.value.name;
					} else if (std::holds_alternative<AST::Import>(value)) {
						name = std::get<AST::Import>(value).name.value.last_fragment().value;
					}

					if (name == fragment.value) {
						// found!
						if (std::holds_alternative<AST::Function>(value)) {
							pointed_item = &std::get<AST::Function>(value);
						} else if (std::holds_alternative<AST::Module>(value)) {
							root_module = &std::get<AST::Module>(value);
						} else if (std::holds_alternative<AST::Import>(value)) {
							// TODO: handle these cases with symbol tables
							AST::Import const* pointed_to_import
								= &std::get<AST::Import>(value);
							if (pointed_to_import->name.value.id.has_value()) {
								std::cout
									<< "TODO: found import to import's id"
									<< std::endl;
							} else {
								std::cout
									<< "TODO: found import to unresolved import"
									<< std::endl;
							}
							std::exit(0);
						}
						found = true;
						break;
					}
				}

				if (!found) {
					std::stringstream title_stream {}, subtitle_stream {};
					title_stream << "unknown identifier '";
					title_stream << import.name.value.path[i].value;
					title_stream << '\'';
					subtitle_stream << "could not find the provided identifier (available: ";
					size_t count = 0;
					for (auto const& item : root_module->body.items) {
						auto const& value = std::get<AST::Module::InnerItem>(item.value);
						std::string name;
						if (std::holds_alternative<AST::Function>(value)) {
							name = std::get<AST::Function>(value).name.value.name;
						} else if (std::holds_alternative<AST::Module>(value)) {
							name = std::get<AST::Module>(value).name.value.name;
						} else if (std::holds_alternative<AST::Import>(value)) {
							name = std::get<AST::Import>(value)
							               .name.value.last_fragment()
							               .value;
						}
						subtitle_stream << '\'' << name << '\'';
						if (++count < root_module->body.items.size()) subtitle_stream << ", ";
					}
					subtitle_stream << ')';
					std::string title    = title_stream.str();
					std::string subtitle = subtitle_stream.str();
					diagnostics_.push_back(
						Diagnostic::error(
							std::move(title),
							std::move(subtitle),
							{Diagnostic::Sample(import.name.value.path[i].span)}
						)
					);
					break;
				}
			}

			if (found) {
				if (pointed_item.has_value())
					import.name.value.id = pointed_item.value()->name.value.id;
				else import.name.value.id = root_module->name.value.id;
				// TODO: throw error if the name wasn't exported
			}
		}
	}
}

void Resolver::populate_unresolved_imports(AST::Module& module) {
	for (Spanned<AST::Module::Item>& item : module.body.items) {
		auto& value = std::get<AST::Module::InnerItem>(item.value);
		if (std::holds_alternative<AST::Import>(value))
			unresolved_imports_.push_back(UnresolvedImport {&std::get<AST::Import>(value), &module, {}});
		else if (std::holds_alternative<AST::Module>(value))
			populate_unresolved_imports(std::get<AST::Module>(value));
	}
}

void Resolver::populate_unresolved_imports() {
	for (ParsedFile& file : parsed_files) { populate_unresolved_imports(file.module); }
}

void Resolver::resolve_identifiers() {}
