#include "resolver.hpp"

#include "levenshtein.hpp"

#include <cstdlib>
#include <iostream>
#include <sstream>
#include <unordered_set>
#include <variant>

#define CLOSEST_THRESHOLD 3
#define CLOSEST_MAX       5

void Resolver::resolve() {
	populate_module_table();
	identify_module_items();
	resolve_identifiers();
}

AST::SymbolID Resolver::next() {
	return counter_++;
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

void Resolver::identify(AST::Identifier& identifier) {
	assert(!identifier.id.has_value());
	identifier.id = {next()};
}

void Resolver::identify(AST::Module& module, FileContext::ID file_id) {
	// TODO: ensure there are no duplicated item names (including imports)
	identify(module.name.value);
	symbol_pool_.push_back(
		Symbol {module.name.value.id.value()[0],
	                file_id,
	                module.name.span,
	                module.name.value.name(),
	                &module,
	                false}
	);
	for (Spanned<AST::Module::Item>& item : module.body.items) {
		auto& value = std::get<AST::Module::InnerItem>(item.value);
		if (std::holds_alternative<AST::Function>(value)) identify(std::get<AST::Function>(value), file_id);
		else if (std::holds_alternative<AST::Module>(value)) identify(std::get<AST::Module>(value), file_id);
	}
}

void Resolver::identify(AST::Function& function, FileContext::ID file_id) {
	identify(function.name.value);
	symbol_pool_.push_back(
		Symbol {function.name.value.id.value()[0],
	                file_id,
	                function.name.span,
	                function.name.value.name(),
	                &function,
	                false}
	);

	// we pre-identify arguments for good measure
	for (auto& argument : function.arguments) {
		// TODO: mutable arguments
		identify(argument.name.value);
		symbol_pool_.push_back(
			Symbol {argument.name.value.id.value()[0],
		                file_id,
		                argument.name.span,
		                argument.name.value.name(),
		                {},
		                false}
		);
	}
}

void Resolver::identify_module_items() {
	for (ParsedFile& file : parsed_files) { identify(file.module, file.file_id); }
}

void Resolver::add_unknown_symbol_diagnostic(
	std::string_view                symbol,
	Span                            span,
	std::vector<std::string> const& possible_symbols,
	std::string_view                scope_type,
	FileContext::ID                 file_id
) {
	std::string       title = std::format("unknown symbol '{}'", symbol);
	std::stringstream subtitle_stream {};
	subtitle_stream << "could not find any symbol with that name in the " << scope_type << " scope";
	std::unordered_set<std::string> symbol_set {};

	std::vector<std::string_view> closest_symbols
		= closest(symbol, possible_symbols, CLOSEST_THRESHOLD, CLOSEST_MAX);

	if (!closest_symbols.empty()) {
		subtitle_stream << " (did you perhaps mean ";
		for (size_t i = 0; i < closest_symbols.size(); ++i) {
			subtitle_stream << '\'' << closest_symbols[i] << '\'';
			if (i + 1 < closest_symbols.size() - 1) subtitle_stream << ", ";
			else if (i + 1 < closest_symbols.size()) subtitle_stream << " or ";
		}
		subtitle_stream << "?)";
	}

	std::string subtitle = subtitle_stream.str();
	parsed_files.at(file_id).diagnostics.push_back(
		Diagnostic::error(
			std::move(title),
			std::move(subtitle),
			{Diagnostic::Sample(get_context(file_id), span)}
		)
	);
}

// FIXME: once more, we don't need this span, since the diagnostics that did need it are gone, maybe we should remove it
void Resolver::resolve(Spanned<AST::Identifier>& identifier, Scope const& scope, FileContext::ID file_id) {
	// do not try to resolve already resolved identifiers (just in case, i don't think we will ever hit this)
	if (identifier.value.id.has_value()) return;

	// unqualified identifiers get resolved differently
	if (identifier.value.is_unqualified()) {
		// Spanned<AST::Identifier> unqualified = qualified_identifier.value.extract_unqualified_with_span();

		Scope const* traversing_scope = &scope;
		while (traversing_scope != nullptr) {
			if (traversing_scope->symbols.contains(identifier.value.name())) {
				std::vector<Symbol*> const& symbols
					= traversing_scope->symbols.at(identifier.value.name());
				identifier.value.id = std::vector<AST::SymbolID> {};
				for (Symbol* symbol : symbols) identifier.value.id.value().push_back(symbol->id);
				return;
			}
			traversing_scope = traversing_scope->parent;
		}

		// if we didn't find anything, throw a diagnostic
		std::unordered_set<std::string> symbol_set {};
		traversing_scope = &scope;
		while (traversing_scope != nullptr) {
			for (auto const& symbol : traversing_scope->symbols) symbol_set.insert(symbol.first);
			traversing_scope = traversing_scope->parent;
		}

		std::vector<std::string> symbol_vector {};
		symbol_vector.reserve(symbol_set.size());
		for (auto it = symbol_set.begin(); it != symbol_set.end();) {
			symbol_vector.push_back(std::move(symbol_set.extract(it++).value()));
		}

		add_unknown_symbol_diagnostic(
			identifier.value.name(),
			identifier.span,
			symbol_vector,
			"current",
			file_id
		);

		return;
	}

	if (!identifier.value.absolute) {
		// TODO: resolve non-absolute qualified identifiers
		std::cout << "unsupported non-absolute qualified identifier detected!" << std::endl;
		return;
	}

	// since this is an absolutely qualified identifier, we must find it in the global scope
	// we do an initial check to ensure that it is in the global scope
	if (!module_table_.contains(identifier.value.path[0].value)) {
		std::vector<std::string> modules {};
		modules.reserve(module_table_.size());
		for (auto const& v : module_table_) modules.push_back(v.first);

		add_unknown_symbol_diagnostic(
			identifier.value.path[0].value,
			identifier.value.path[0].span,
			modules,
			"global",
			file_id
		);

		return;
	}

	// the module to check within
	AST::Module const* root_module = module_table_.at(identifier.value.path[0].value);
	// the item(s) found after traversal
	std::vector<Symbol*> pointed_items {};
	// the id for each of the pointed items
	std::vector<AST::SymbolID> pointed_ids {};
	// set this once we reach a function or something decidedly without subitems
	bool cannot_traverse_further = false;

	for (size_t i = 1; i < identifier.value.path.size(); ++i) {
		Spanned<std::string> const& fragment = identifier.value.path[i];
		if (cannot_traverse_further) {
			// for now, this can only mean it is a function, which does not have subitems
			std::cout << "tried to access a function's subitems !" << std::endl;
			break;
		}

		// we reset each cycle so that we don't end up racking up all of the pointed items
		pointed_items = {};
		pointed_ids   = {};
		for (auto const& item : root_module->body.items) {
			auto const& value = std::get<AST::Module::InnerItem>(item.value);
			std::string name  = AST::Module::get_name(item.value);

			if (name == fragment.value) {
				// found!
				size_t added_items = 0;
				if (std::holds_alternative<AST::Function>(value)) {
					assert(std::get<AST::Function>(value).name.value.id.has_value());
					for (AST::SymbolID id : std::get<AST::Function>(value).name.value.id.value()) {
						pointed_items.push_back(&get_single_symbol(id));
						added_items++;
					}
					cannot_traverse_further = true;
				} else if (std::holds_alternative<AST::Module>(value)) {
					root_module = &std::get<AST::Module>(value);
					pointed_items.push_back(
						&get_single_symbol(std::get<AST::Module>(value).name.value)
					);
				} else if (std::holds_alternative<AST::Import>(value)) {
					auto const& import = std::get<AST::Import>(value);
					// TODO: do something else about this & handle imports that import other imports
					if (!import.name.value.id.has_value()) {
						std::cout << "unresolved import !" << std::endl;
						break;
					}
					for (AST::SymbolID id : import.name.value.id.value()) {
						pointed_items.push_back(&get_single_symbol(id));
						added_items++;

						// change the root module if we are definitely pointing towards a module
						if (std::holds_alternative<AST::Module*>(
							    pointed_items.at(pointed_items.size() - 1)->item
						    ))
							root_module = &*std::get<AST::Module*>(
								pointed_items.at(pointed_items.size() - 1)->item
							);
					}
				}

				for (size_t j = 1; j <= added_items; ++j)
					pointed_ids.push_back(pointed_items.at(pointed_items.size() - j)->id);
			}
		}

		if (pointed_items.empty()) {
			std::vector<std::string> possibilities {};
			possibilities.reserve(root_module->body.items.size());
			for (auto const& item : root_module->body.items)
				possibilities.push_back(AST::Module::get_name(item.value));

			add_unknown_symbol_diagnostic(
				identifier.value.path[i].value,
				identifier.value.path[i].span,
				possibilities,
				"specified",
				file_id
			);

			break;
		}
	}

	// if we didn't find anything, we intentionally pass on the empty vector to the identifier
	identifier.value.id = pointed_ids;
}

void Resolver::resolve(AST::Expression::UnaryOperation& unary_operation, Scope const& scope, FileContext::ID file_id) {
	resolve(*unary_operation.operand, scope, file_id);
}

void Resolver::resolve(
	AST::Expression::BinaryOperation& binary_operation,
	Scope const&                      scope,
	FileContext::ID                   file_id
) {
	resolve(*binary_operation.lhs, scope, file_id);
	resolve(*binary_operation.rhs, scope, file_id);
}

void Resolver::resolve(AST::Expression::FunctionCall& function_call, Scope const& scope, FileContext::ID file_id) {
	resolve(*function_call.callee, scope, file_id);
	for (auto& argument : function_call.arguments.ordered) { resolve(argument, scope, file_id); }
	// we cannot resolve labels just yet, because we need to know the type of the function and signature and etc
	for (auto& argument : function_call.arguments.labeled) { resolve(std::get<1>(argument), scope, file_id); }
}

void Resolver::resolve(AST::Expression& expression, Span span, Scope const& scope, FileContext::ID file_id) {
	switch (expression.kind()) {
	case AST::Expression::Kind::UnaryOperation:  resolve(expression.get_unary_operation(), scope, file_id); return;
	case AST::Expression::Kind::BinaryOperation: resolve(expression.get_binary_operation(), scope, file_id); return;
	case AST::Expression::Kind::FunctionCall:    resolve(expression.get_function_call(), scope, file_id); return;
	case AST::Expression::Kind::Atom:            break;
	}

	// atom resolution (better to do it here directly)
	AST::Expression::Atom& atom = expression.get_atom();
	if (atom.kind() == AST::Expression::Atom::Kind::Identifier) {
		// for identifiers, we create a faux spanned qualified identifier, resolve it and apply that information
		// here
		Spanned<AST::Identifier> qualified_identifier {span, atom.get_identifier()};
		resolve(qualified_identifier, scope, file_id);
		atom.get_identifier().id = qualified_identifier.value.id;
	} else if (atom.kind() == AST::Expression::Atom::Kind::Expression) {
		// for subexpressions, we just recurse
		resolve(*atom.get_expression(), span, scope, file_id);
		return;
	}
}

void Resolver::resolve(Spanned<AST::Expression>& expression, Scope const& scope, FileContext::ID file_id) {
	resolve(expression.value, expression.span, scope, file_id);
}

void Resolver::resolve(AST::Statement::Declare& declare, Scope& scope, FileContext::ID file_id) {
	// resolve the value before the name, otherwise 'const a = a;' would not work
	if (declare.value.has_value()) resolve(declare.value.value(), scope, file_id);
	identify(declare.name.value);
	symbol_pool_.push_back(
		Symbol {declare.name.value.id.value()[0],
	                file_id,
	                declare.name.span,
	                declare.name.value.name(),
	                {},
	                declare.mutable_.value}
	);
	// intentionally replace (shadowing)
	scope.symbols.insert_or_assign(
		declare.name.value.name(),
		std::vector<Symbol*> {&get_single_symbol(declare.name.value)}
	);
}

void Resolver::resolve(AST::Statement::Set& set, Scope& scope, FileContext::ID file_id) {
	// TODO: maybe check here for mutability?
	resolve(set.lhs, scope, file_id);
	resolve(set.rhs, scope, file_id);
}

void Resolver::resolve(AST::Statement::Return& return_, Scope& scope, FileContext::ID file_id) {
	if (return_.value.has_value()) resolve(return_.value.value(), scope, file_id);
}

void Resolver::resolve(Spanned<AST::Statement>& statement, Scope& scope, FileContext::ID file_id) {
	switch (statement.value.kind()) {
	case AST::Statement::Kind::Declare: resolve(statement.value.get_declare(), scope, file_id); return;
	case AST::Statement::Kind::Set:     resolve(statement.value.get_set(), scope, file_id); return;
	case AST::Statement::Kind::Expression:
		resolve(statement.value.get_expression(), statement.span, scope, file_id);
		return;
	case AST::Statement::Kind::Return: resolve(statement.value.get_return(), scope, file_id); return;
	case AST::Statement::Kind::Scope:  resolve(statement.value.get_scope(), scope, file_id); return;
	}
}

void Resolver::resolve(AST::Scope& ast_scope, Scope resolver_scope, FileContext::ID file_id) {
	for (auto& statement : ast_scope) { resolve(statement, resolver_scope, file_id); }
}

void Resolver::resolve(AST::Function& function, Scope scope, FileContext::ID file_id) {
	Scope child_scope {&scope, {}};
	for (auto& argument : function.arguments) {
		// intentionally replace (shadowing)
		child_scope.symbols.insert_or_assign(
			argument.name.value.name(),
			std::vector<Symbol*> {&get_single_symbol(argument.name.value)}
		);
	}
	if (function.body.has_value()) resolve(function.body.value(), child_scope, file_id);
}

void Resolver::resolve(AST::Module& module, Scope scope, FileContext::ID file_id) {
	Scope child_scope {&scope, {}};
	for (Spanned<AST::Module::Item>& item : module.body.items) {
		auto& value = std::get<AST::Module::InnerItem>(item.value);
		if (std::holds_alternative<AST::Function>(value)) {
			auto& function = std::get<AST::Function>(value);
			if (child_scope.symbols.contains(function.name.value.name()))
				child_scope.symbols.at(function.name.value.name())
					.push_back(&get_single_symbol(function.name.value));
			else
				child_scope.symbols.emplace(
					function.name.value.name(),
					std::vector<Symbol*> {&get_single_symbol(function.name.value)}
				);
		} else if (std::holds_alternative<AST::Module>(value)) {
			auto& submodule = std::get<AST::Module>(value);
			child_scope.symbols.emplace(
				submodule.name.value.name(),
				std::vector<Symbol*> {&get_single_symbol(submodule.name.value)}
			);
		} else if (std::holds_alternative<AST::Import>(value)) {
			auto& import = std::get<AST::Import>(value);
			if (!import.name.value.absolute) continue;
			resolve(import.name, child_scope, file_id);
			if (!import.name.value.id.has_value() || import.name.value.id.value().empty()) continue;
			child_scope.symbols.emplace(import.name.value.last_fragment().value, std::vector<Symbol*> {});
			for (AST::SymbolID id : import.name.value.id.value()) {
				child_scope.symbols.at(import.name.value.last_fragment().value)
					.push_back(&symbol_pool_.at(id));
			}
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
