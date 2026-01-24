#include "resolver.hpp"

#include "ast/identifier.hpp"

#include <cstdlib>
#include <iostream>
#include <sstream>
#include <unordered_set>
#include <variant>

void Resolver::resolve() {
	populate_module_table();
	identify_module_items();
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

void Resolver::identify(AST::Module& module, uint32_t file_id) {
	// TODO: ensure there are no duplicated item names (including imports)
	identify(module.name.value);
	symbol_pool_.push_back(
		Symbol {module.name.value.id.value(), file_id, module.name.span, module.name.value.name, &module, false}
	);
	for (Spanned<AST::Module::Item>& item : module.body.items) {
		auto& value = std::get<AST::Module::InnerItem>(item.value);
		if (std::holds_alternative<AST::Function>(value)) identify(std::get<AST::Function>(value), file_id);
		else if (std::holds_alternative<AST::Module>(value)) identify(std::get<AST::Module>(value), file_id);
	}
}

void Resolver::identify(AST::Function& function, uint32_t file_id) {
	identify(function.name.value);
	symbol_pool_.push_back(
		Symbol {function.name.value.id.value(),
	                file_id,
	                function.name.span,
	                function.name.value.name,
	                &function,
	                false}
	);
}

void Resolver::identify_module_items() {
	for (ParsedFile& file : parsed_files) { identify(file.module, file.file_id); }
}

void Resolver::resolve(Spanned<AST::Identifier>& identifier, Scope const& lookup_scope, uint32_t file_id) {
	if (identifier.value.id.has_value()) return;

	Scope const* scope = &lookup_scope;
	while (scope != nullptr) {
		if (scope->symbols.contains(identifier.value.name)) {
			identifier.value.id = scope->symbols.at(identifier.value.name)->id;
			break;
		}
		scope = scope->parent;
	}

	if (scope == nullptr) {
		std::string       title = std::format("unknown symbol '{}'", identifier.value.name);
		std::stringstream subtitle_stream {};
		subtitle_stream << "could not find any symbol with that name in the current scope (available: ";
		std::unordered_set<std::string> available_symbols {};
		scope = &lookup_scope;
		while (scope != nullptr) {
			for (auto const& symbol : scope->symbols) available_symbols.insert(symbol.first);
			scope = scope->parent;
		}
		size_t count = 0;
		for (auto const& symbol : available_symbols) {
			subtitle_stream << '\'' << symbol << '\'';
			if (++count < available_symbols.size()) subtitle_stream << ", ";
		}
		subtitle_stream << ')';
		std::string subtitle = subtitle_stream.str();
		parsed_files.at(file_id).diagnostics.push_back(
			Diagnostic::error(std::move(title), std::move(subtitle), {Diagnostic::Sample(identifier.span)})
		);
	}
}

void Resolver::resolve(AST::QualifiedIdentifier& qualified_identifier, Scope const& scope, uint32_t file_id) {
	if (qualified_identifier.is_unqualified()) {
		Spanned<AST::Identifier> unqualified = qualified_identifier.get_unqualified();
		resolve(unqualified, scope, file_id);
		qualified_identifier.id = unqualified.value.id;
		return;
	}

	if (!qualified_identifier.absolute) {
		// TODO: resolve non-absolute qualified identifiers
		std::cout << "unsupported non-absolute qualified identifier detected!" << std::endl;
		return;
	}

	// since this is an absolutely qualified identifier, we must find it in the global scope
	// we do an initial check to ensure that it is in the global scope
	if (!module_table_.contains(qualified_identifier.path[0].value)) {
		std::stringstream title_stream {}, subtitle_stream {};
		title_stream << "unknown symbol '";
		title_stream << qualified_identifier.path[0].value;
		title_stream << '\'';
		subtitle_stream << "could not find any symbol with that name in the global scope (available: ";
		size_t count = 0;
		for (auto const& v : module_table_) {
			subtitle_stream << '\'' << v.first << '\'';
			if (++count < module_table_.size()) subtitle_stream << ", ";
		}
		subtitle_stream << ')';
		std::string title    = title_stream.str();
		std::string subtitle = subtitle_stream.str();
		parsed_files.at(file_id).diagnostics.push_back(
			Diagnostic::error(
				std::move(title),
				std::move(subtitle),
				{Diagnostic::Sample(qualified_identifier.path[0].span)}
			)
		);
		return;
	}

	// the module to check within
	AST::Module const* root_module = module_table_.at(qualified_identifier.path[0].value);
	// the last item that was pointed to
	Symbol* pointed_item = nullptr;
	// whether we have already thrown the privacy violation diagnostic
	bool privacy_violated_diagnostic_thrown = false;
	// whether we found anything last iteration
	bool found = false;
	// set this once we reach a function or something decidedly without subitems
	bool cannot_traverse_further = false;

	for (size_t i = 1; i < qualified_identifier.path.size(); ++i) {
		Spanned<std::string> const& fragment = qualified_identifier.path[i];
		if (cannot_traverse_further) {
			// for now, this can only mean it is a function, which does not have subitems
			std::cout << "tried to access a function's subitems !" << std::endl;
			break;
		}
		found = false;
		for (auto const& item : root_module->body.items) {
			auto const& value = std::get<AST::Module::InnerItem>(item.value);
			std::string name  = AST::Module::get_name(item.value);

			if (name == fragment.value) {
				// found!
				if (std::holds_alternative<AST::Function>(value)) {
					pointed_item = &symbol_pool_.at(
						std::get<AST::Function>(value).name.value.id.value()
					);
					cannot_traverse_further = true;
				} else if (std::holds_alternative<AST::Module>(value)) {
					root_module = &std::get<AST::Module>(value);
					pointed_item
						= &symbol_pool_.at(std::get<AST::Module>(value).name.value.id.value());
				} else if (std::holds_alternative<AST::Import>(value)) {
					auto const& import = std::get<AST::Import>(value);
					// TODO: do something else about this & handle imports that import other imports
					if (!import.name.value.id.has_value()) {
						std::cout << "unresolved import !" << std::endl;
						break;
					}
					pointed_item = &symbol_pool_.at(import.name.value.id.value());
					// change the root module if we are definitely pointing towards a module
					if (std::holds_alternative<AST::Module*>(pointed_item->item))
						root_module = &*std::get<AST::Module*>(pointed_item->item);
				}
				if (!std::get<bool>(item.value) && !privacy_violated_diagnostic_thrown) {
					// this item is not exported, but we're trying to access it
					// TODO: check if it is in the same scope as ours or on a parent module(?) to
					// not raise the diagnostic
					std::string title = std::format("tried to access unexported item '{}'", name);
					// FIXME: diagnostics are not being thrown showing the correct files
					parsed_files.at(file_id).diagnostics.push_back(
						Diagnostic::error(
							std::move(title),
							"the item must be exported for it to be accessible from outside its scope",
							{Diagnostic::Sample {{Diagnostic::Sample::Label {
								 pointed_item->span,
								 "item declared here",
								 OutFmt::Color::BrightBlue
							 }}},
					                 Diagnostic::Sample {{Diagnostic::Sample::Label {
								 Span(qualified_identifier.path[0].span.start,
					                              fragment.span.end),
								 "item accessed here",
								 OutFmt::Color::BrightMagenta
							 }}}}
						)
					);
					privacy_violated_diagnostic_thrown = true;
				}
				found = true;
				break;
			}
		}

		if (!found) {
			std::stringstream title_stream {}, subtitle_stream {};
			title_stream << "unknown symbol '";
			title_stream << qualified_identifier.path[i].value;
			title_stream << '\'';
			subtitle_stream
				<< "could not find any symbol with that name in the specified scope (available: ";
			size_t count = 0;
			for (auto const& item : root_module->body.items) {
				std::string name = AST::Module::get_name(item.value);
				subtitle_stream << '\'' << name << '\'';
				if (++count < root_module->body.items.size()) subtitle_stream << ", ";
			}
			subtitle_stream << ')';
			std::string title    = title_stream.str();
			std::string subtitle = subtitle_stream.str();
			parsed_files.at(file_id).diagnostics.push_back(
				Diagnostic::error(
					std::move(title),
					std::move(subtitle),
					{Diagnostic::Sample(qualified_identifier.path[i].span)}
				)
			);
			break;
		}
	}

	if (!found) return;

	assert(pointed_item);
	qualified_identifier.id = pointed_item->id;

	// TODO: throw error if the name wasn't exported
}

void Resolver::resolve(AST::Expression::Atom& atom, Scope const& scope, uint32_t file_id) {
	switch (atom.kind()) {
	case AST::Expression::Atom::Kind::Identifier:    resolve(atom.get_identifier(), scope, file_id); return;
	case AST::Expression::Atom::Kind::Expression:    resolve(*atom.get_expression(), scope, file_id); return;
	case AST::Expression::Atom::Kind::NumberLiteral:
	case AST::Expression::Atom::Kind::StringLiteral:
	case AST::Expression::Atom::Kind::CharLiteral:   break;
	}
}

void Resolver::resolve(AST::Expression::UnaryOperation& unary_operation, Scope const& scope, uint32_t file_id) {
	resolve(unary_operation.operand->value, scope, file_id);
}

void Resolver::resolve(AST::Expression::BinaryOperation& binary_operation, Scope const& scope, uint32_t file_id) {
	resolve(binary_operation.lhs->value, scope, file_id);
	resolve(binary_operation.rhs->value, scope, file_id);
}

void Resolver::resolve(AST::Expression::FunctionCall& function_call, Scope const& scope, uint32_t file_id) {
	resolve(function_call.callee->value, scope, file_id);
	for (auto& argument : function_call.arguments.ordered) { resolve(argument.value, scope, file_id); }
	// FIXME: we need to properly resolve these labels wrt. func args, for which we need to pre-identify func args
	for (auto& argument : function_call.arguments.labeled) { resolve(std::get<1>(argument).value, scope, file_id); }
}

void Resolver::resolve(AST::Expression& expression, Scope const& scope, uint32_t file_id) {
	switch (expression.kind()) {
	case AST::Expression::Kind::Atom:            resolve(expression.get_atom(), scope, file_id); return;
	case AST::Expression::Kind::UnaryOperation:  resolve(expression.get_unary_operation(), scope, file_id); return;
	case AST::Expression::Kind::BinaryOperation: resolve(expression.get_binary_operation(), scope, file_id); return;
	case AST::Expression::Kind::FunctionCall:    resolve(expression.get_function_call(), scope, file_id); return;
	}
}

void Resolver::resolve(AST::Statement::Declare& declare, Scope& scope, uint32_t file_id) {
	// resolve the value before the name, otherwise 'const a = a;' would not work
	if (declare.value.has_value()) resolve(declare.value.value().value, scope, file_id);
	identify(declare.name.value);
	symbol_pool_.push_back(
		Symbol {declare.name.value.id.value(),
	                file_id,
	                declare.name.span,
	                declare.name.value.name,
	                {},
	                declare.mutable_.value}
	);
	scope.symbols.insert_or_assign(declare.name.value.name, &symbol_pool_.at(declare.name.value.id.value()));
}

void Resolver::resolve(AST::Statement::Set& set, Scope& scope, uint32_t file_id) {
	// TODO: maybe check here for mutability?
	resolve(set.lhs.value, scope, file_id);
	resolve(set.rhs.value, scope, file_id);
}

void Resolver::resolve(AST::Statement::Return& return_, Scope& scope, uint32_t file_id) {
	if (return_.value.has_value()) resolve(return_.value.value().value, scope, file_id);
}

void Resolver::resolve(AST::Statement& statement, Scope& scope, uint32_t file_id) {
	switch (statement.kind()) {
	case AST::Statement::Kind::Declare:    resolve(statement.get_declare(), scope, file_id); return;
	case AST::Statement::Kind::Set:        resolve(statement.get_set(), scope, file_id); return;
	case AST::Statement::Kind::Expression: resolve(statement.get_expression(), scope, file_id); return;
	case AST::Statement::Kind::Return:     resolve(statement.get_return(), scope, file_id); return;
	case AST::Statement::Kind::Scope:      resolve(statement.get_scope(), scope, file_id); return;
	}
}

void Resolver::resolve(AST::Scope& ast_scope, Scope resolver_scope, uint32_t file_id) {
	for (auto& statement : ast_scope) { resolve(statement.value, resolver_scope, file_id); }
}

void Resolver::resolve(AST::Function& function, Scope scope, uint32_t file_id) {
	Scope child_scope {&scope, {}};
	for (auto& argument : function.arguments) {
		identify(argument.name.value);
		// TODO: support mutable arguments
		symbol_pool_.push_back(
			Symbol {argument.name.value.id.value(),
		                file_id,
		                argument.name.span,
		                argument.name.value.name,
		                {},
		                false}
		);
		child_scope.symbols.emplace(argument.name.value.name, &symbol_pool_.at(argument.name.value.id.value()));
	}
	if (function.body.has_value()) resolve(function.body.value(), child_scope, file_id);
}

void Resolver::resolve(AST::Module& module, Scope scope, uint32_t file_id) {
	Scope child_scope {&scope, {}};
	for (Spanned<AST::Module::Item>& item : module.body.items) {
		auto& value = std::get<AST::Module::InnerItem>(item.value);
		if (std::holds_alternative<AST::Function>(value)) {
			auto& function = std::get<AST::Function>(value);
			child_scope.symbols.emplace(
				function.name.value.name,
				&symbol_pool_.at(function.name.value.id.value())
			);
		} else if (std::holds_alternative<AST::Module>(value)) {
			auto& submodule = std::get<AST::Module>(value);
			child_scope.symbols.emplace(
				submodule.name.value.name,
				&symbol_pool_.at(submodule.name.value.id.value())
			);
		} else if (std::holds_alternative<AST::Import>(value)) {
			auto& import = std::get<AST::Import>(value);
			if (!import.name.value.absolute) continue;
			resolve(import.name.value, child_scope, file_id);
			if (!import.name.value.id.has_value()) continue;
			child_scope.symbols.emplace(
				import.name.value.last_fragment().value,
				&symbol_pool_.at(import.name.value.id.value())
			);
		}
	}

	for (Spanned<AST::Module::Item>& item : module.body.items) {
		auto& value = std::get<AST::Module::InnerItem>(item.value);
		if (std::holds_alternative<AST::Function>(value))
			resolve(std::get<AST::Function>(value), child_scope, file_id);
		else if (std::holds_alternative<AST::Module>(value))
			resolve(std::get<AST::Module>(value), child_scope, file_id);
	}
}

void Resolver::resolve_identifiers() {
	for (ParsedFile& file : parsed_files) { resolve(file.module, Scope {}, file.file_id); }
}
