#include "levenshtein.hpp"
#include "resolver.hpp"

#include <cstdlib>
#include <iostream>
#include <sstream>
#include <unordered_set>
#include <variant>

#define CLOSEST_THRESHOLD 3
#define CLOSEST_MAX       5

AST::SymbolID Resolver::next() {
	assert(symbol_pool_.size() == counter_);
	return counter_++;
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

void Resolver::resolve(AST::Identifier& identifier, Span span, Scope const& scope, FileContext::ID file_id) {
	// do not try to resolve already resolved identifiers (just in case, i don't think we will ever hit this)
	if (identifier.id.has_value()) return;

	// unqualified identifiers get resolved differently
	if (identifier.is_unqualified()) {
		Scope const* traversing_scope = &scope;
		while (traversing_scope != nullptr) {
			if (traversing_scope->symbols.contains(identifier.name())) {
				identifier.id = traversing_scope->symbols.at(identifier.name());
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

		add_unknown_symbol_diagnostic(identifier.name(), span, symbol_vector, "current", file_id);

		return;
	}

	if (!identifier.absolute) {
		// TODO: resolve non-absolute qualified identifiers
		std::cout << "unsupported non-absolute qualified identifier detected!" << std::endl;
		return;
	}

	// since this is an absolutely qualified identifier, we must find it in the global scope
	// we do an initial check to ensure that it is in the global scope
	if (!module_table_.contains(identifier.path[0].value)) {
		std::vector<std::string> modules {};
		modules.reserve(module_table_.size());
		for (auto const& v : module_table_) modules.push_back(v.first);

		add_unknown_symbol_diagnostic(
			identifier.path[0].value,
			identifier.path[0].span,
			modules,
			"global",
			file_id
		);

		return;
	}

	// the module to check within
	AST::Module const* root_module = module_table_.at(identifier.path[0].value);
	// the item(s) found after traversal
	std::vector<Symbol*> pointed_items {};
	// the id for each of the pointed items
	std::vector<AST::SymbolID> pointed_ids {};
	// set this once we reach a function or something decidedly without subitems
	bool cannot_traverse_further = false;

	for (size_t i = 1; i < identifier.path.size(); ++i) {
		Spanned<std::string> const& fragment = identifier.path[i];
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
				} else if (std::holds_alternative<AST::Alias>(value)) {
					auto const& alias = std::get<AST::Alias>(value);
					// TODO: do something else about this & handle aliases that alias other aliases
					if (!alias.value.value.id.has_value()) {
						std::cout << "unresolved alias !" << std::endl;
						break;
					}
					for (AST::SymbolID id : alias.value.value.id.value()) {
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
				identifier.path[i].value,
				identifier.path[i].span,
				possibilities,
				"specified",
				file_id
			);

			break;
		}
	}

	// if we didn't find anything, we intentionally pass on the empty vector to the identifier
	identifier.id = pointed_ids;
}

void Resolver::resolve(Spanned<AST::Identifier>& identifier, Scope const& scope, FileContext::ID file_id) {
	return resolve(identifier.value, identifier.span, scope, file_id);
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
		// identifiers need to be resolved
		resolve(atom.get_identifier(), span, scope, file_id);
	} else if (atom.kind() == AST::Expression::Atom::Kind::Expression) {
		// for subexpressions, we just recurse
		resolve(*atom.get_expression(), span, scope, file_id);
	}
	// the rest of them do not need any kind of resolution yet
	return;
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
	                register_type(
				declare.type.has_value() ? TypeInfo::from_type(declare.type.value().value)
							 : TypeInfo::make_unknown(),
				declare.type.has_value() ? declare.type.value().span : declare.name.span,
				file_id,
				declare.name.value.id.value()[0]
			),
	                declare.mutable_.value}
	);
	// intentionally replace (shadowing)
	scope.symbols.insert_or_assign(declare.name.value.name(), declare.name.value.id.value());
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
		child_scope.symbols.insert_or_assign(argument.name.value.name(), argument.name.value.id.value());
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
					.push_back(function.name.value.id.value()[0]);
			else child_scope.symbols.emplace(function.name.value.name(), function.name.value.id.value());
		} else if (std::holds_alternative<AST::Module>(value)) {
			auto& submodule = std::get<AST::Module>(value);
			child_scope.symbols.emplace(submodule.name.value.name(), submodule.name.value.id.value());
		} else if (std::holds_alternative<AST::Alias>(value)) {
			auto& alias = std::get<AST::Alias>(value);
			resolve(alias.value, child_scope, file_id);
			if (!alias.value.value.id.has_value() || alias.value.value.id.value().empty()) continue;
			child_scope.symbols.emplace(alias.name.value.name(), alias.value.value.id.value());
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
