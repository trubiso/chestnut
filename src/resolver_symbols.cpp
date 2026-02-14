#include "levenshtein.hpp"
#include "resolver.hpp"

#include <cstdlib>
#include <iostream>
#include <sstream>
#include <unordered_set>
#include <variant>

#define CLOSEST_THRESHOLD 3
#define CLOSEST_MAX       5

bool Resolver::Symbol::is_visible(FileContext::ID other_id) const {
	// if we're in the same file, it is trivially visible
	if (file_id == other_id) return true;
	// if it has been imported from the other file, it is visible
	if (std::find(imported_from.cbegin(), imported_from.cend(), other_id) != imported_from.cend()) return true;
	// otherwise, it is never visible
	return false;
}

AST::SymbolID Resolver::symbol_next() {
	assert(symbol_pool_.size() == symbol_counter_);
	return symbol_counter_++;
}

void Resolver::add_unknown_symbol_diagnostic(
	std::string_view                symbol,
	Span                            span,
	std::vector<std::string> const& possible_symbols,
	std::string_view                scope_type,
	FileContext::ID                 file_id,
	bool                            add_import_suggestion
) {
	std::string       title = std::format("unknown symbol '{}'", symbol);
	std::stringstream subtitle_stream {};
	subtitle_stream << "could not find any symbol with that name in the " << scope_type << " scope";

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

	if (add_import_suggestion)
		subtitle_stream
			<< ". an unimported symbol with that name does exist, you might have forgotten to import it!";

	std::string subtitle = subtitle_stream.str();
	parsed_files.at(file_id).diagnostics.push_back(
		Diagnostic::error(
			std::move(title),
			std::move(subtitle),
			{Diagnostic::Sample(get_context(file_id), span)}
		)
	);
}

void Resolver::resolve(
	AST::Identifier& identifier,
	Span             span,
	Scope const&     scope,
	FileContext::ID  file_id,
	bool             include_unimported
) {
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
		std::transform(
			module_table_.cbegin(),
			module_table_.cend(),
			std::back_inserter(modules),
			[](auto const& v) { return v.first; }
		);

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
			std::cout << "tried to access a function/struct's subitems !" << std::endl;
			break;
		}

		// we reset each cycle so that we don't end up racking up all of the pointed items
		pointed_items           = {};
		pointed_ids             = {};
		size_t actual_additions = 0;
		for (auto const& item : root_module->body.items) {
			auto const& value = std::get<AST::Module::InnerItem>(item.value);
			if (std::holds_alternative<AST::Import>(value)) continue;
			std::string name = AST::Module::get_name(item.value);

			if (name != fragment.value) continue;

			// found!
			size_t added_items = 0;
			if (std::holds_alternative<AST::Function>(value)) {
				assert(std::get<AST::Function>(value).name.value.id.has_value());
				for (AST::SymbolID id : std::get<AST::Function>(value).name.value.id.value()) {
					pointed_items.push_back(&get_single_symbol(id));
					added_items++;
				}
				cannot_traverse_further = true;
			} else if (std::holds_alternative<AST::Struct>(value)) {
				pointed_items.push_back(&get_single_symbol(std::get<AST::Struct>(value).name.value));
				added_items++;
				cannot_traverse_further = true;
			} else if (std::holds_alternative<AST::Module>(value)) {
				root_module = &std::get<AST::Module>(value);
				pointed_items.push_back(&get_single_symbol(std::get<AST::Module>(value).name.value));
				actual_additions++;
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

			// only include imported items
			for (size_t j = 1; j <= added_items; ++j) {
				Symbol* symbol        = pointed_items.at(pointed_items.size() - j);
				bool    should_import = include_unimported || symbol->is_visible(file_id);
				// only apply these conditions to the tip of the identifier
				if (i + 1 < identifier.path.size() || should_import) {
					pointed_ids.push_back(symbol->id);
					actual_additions++;
				}
			}
		}

		if (!actual_additions) {
			std::vector<std::string> possibilities {};
			possibilities.reserve(root_module->body.items.size());
			bool unimported_same_name_item_exists = false;
			for (auto const& item : root_module->body.items) {
				auto const& value = std::get<AST::Module::InnerItem>(item.value);
				if (std::holds_alternative<AST::Import>(value)) continue;
				std::optional<std::vector<AST::SymbolID>> const* symbol_id = nullptr;
				if (std::holds_alternative<AST::Function>(value)) {
					symbol_id = &std::get<AST::Function>(value).name.value.id;
				} else if (std::holds_alternative<AST::Module>(value)) {
					symbol_id = &std::get<AST::Module>(value).name.value.id;
				} else if (std::holds_alternative<AST::Alias>(value)) {
					symbol_id = &std::get<AST::Alias>(value).value.value.id;
				} else if (std::holds_alternative<AST::Struct>(value)) {
					symbol_id = &std::get<AST::Struct>(value).name.value.id;
				} else [[assume(false)]];

				if (!symbol_id->has_value()) continue;
				bool any_are_visible = false;
				for (AST::SymbolID id : symbol_id->value()) {
					if (symbol_pool_.at(id).is_visible(file_id)) {
						any_are_visible = true;
						break;
					} else if (symbol_pool_.at(id).name == fragment.value)
						unimported_same_name_item_exists = true;
				}
				if (!any_are_visible) continue;

				possibilities.push_back(AST::Module::get_name(item.value));
			}

			add_unknown_symbol_diagnostic(
				identifier.path[i].value,
				identifier.path[i].span,
				possibilities,
				"specified",
				file_id,
				unimported_same_name_item_exists
			);

			break;
		}
	}

	// if we didn't find anything, we intentionally pass on the empty vector to the identifier
	identifier.id = pointed_ids;
}

void Resolver::resolve(AST::Type::Atom& atom, Span span, Scope const& scope, FileContext::ID file_id) {
	if (!atom.is_named()) return;
	resolve(atom.get_named(), span, scope, file_id);

	// check that we actually got a type
	if (atom.get_named().id.has_value()) {
		if (atom.get_named().id.value().empty()) return;
		if (atom.get_named().id.value().size() > 1) {
			// TODO: diagnostic (although this likely happens if you use a function name as a type name, in
			// which case we should punish the author of the code)
			std::cout << "ambiguous type, what did you do???" << std::endl;
			std::exit(0);
		}
		if (!std::holds_alternative<AST::Struct*>(get_single_symbol(atom.get_named()).item)) {
			// TODO: proper diagnostic instead of guilt-tripping
			std::cout << "why did you use a non-struct as a type? :(" << std::endl;
			std::exit(0);
		}
	}
}

void Resolver::resolve(AST::Type& type, Span span, Scope const& scope, FileContext::ID file_id) {
	switch (type.kind()) {
	case AST::Type::Kind::Atom:    return resolve(type.get_atom(), span, scope, file_id);
	case AST::Type::Kind::Pointer: return resolve(type.get_pointer().type->value, span, scope, file_id);
	}
}

void Resolver::resolve(Spanned<AST::Type>& type, Scope const& scope, FileContext::ID file_id) {
	return resolve(type.value, type.span, scope, file_id);
}

void Resolver::resolve(
	Spanned<AST::Identifier>& identifier,
	Scope const&              scope,
	FileContext::ID           file_id,
	bool                      include_unimported
) {
	return resolve(identifier.value, identifier.span, scope, file_id, include_unimported);
}

void Resolver::resolve(AST::Expression::UnaryOperation& unary_operation, Scope const& scope, FileContext::ID file_id) {
	resolve(*unary_operation.operand, scope, file_id);
}

void Resolver::resolve(
	AST::Expression::AddressOperation& address_operation,
	Scope const&                       scope,
	FileContext::ID                    file_id
) {
	resolve(*address_operation.operand, scope, file_id);
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
	for (auto& argument : function_call.arguments.labeled) { resolve(std::get<1>(argument), scope, file_id); }
}

void Resolver::resolve(AST::Expression::MemberAccess& member_access, Scope const& scope, FileContext::ID file_id) {
	resolve(*member_access.accessee, scope, file_id);
}

void Resolver::resolve(AST::Expression& expression, Span span, Scope const& scope, FileContext::ID file_id) {
	switch (expression.kind()) {
	case AST::Expression::Kind::UnaryOperation: resolve(expression.get_unary_operation(), scope, file_id); return;
	case AST::Expression::Kind::AddressOperation:
		resolve(expression.get_address_operation(), scope, file_id);
		return;
	case AST::Expression::Kind::BinaryOperation: resolve(expression.get_binary_operation(), scope, file_id); return;
	case AST::Expression::Kind::FunctionCall:    resolve(expression.get_function_call(), scope, file_id); return;
	case AST::Expression::Kind::Atom:            break;
	case AST::Expression::Kind::If:              return;
	case AST::Expression::Kind::MemberAccess:    resolve(expression.get_member_access(), scope, file_id); return;
	}

	// atom resolution (better to do it here directly)
	AST::Expression::Atom& atom = expression.get_atom();
	if (atom.is_identifier()) {
		// identifiers need to be resolved
		resolve(atom.get_identifier(), span, scope, file_id);
	} else if (atom.is_expression()) {
		// for subexpressions, we just recurse
		resolve(*atom.get_expression(), span, scope, file_id);
	} else if (atom.is_struct_literal()) {
		// for struct literals, we need to resolve both the type and the fields
		AST::Expression::Atom::StructLiteral& struct_literal = atom.get_struct_literal();

		// we transfer the name to a type to resolve it and then back to maintain the ID
		// in the future, we will have to ensure that this is a struct, but for now resolve already does that
		AST::Type temp_type
			= AST::Type::make_atom(AST::Type::Atom::make_named(std::move(struct_literal.name.value)));
		resolve(temp_type, struct_literal.name.span, scope, file_id);
		struct_literal.name.value = std::move(temp_type.get_atom().get_named());

		// now, we check if all given fields exist in the struct and we resolve all of them, regardless of
		// whether they exist or not
		AST::Struct* struct_
			= std::get<AST::Struct*>(symbol_pool_.at(struct_literal.name.value.id.value().at(0)).item);
		for (auto& field : struct_literal.fields) {
			// TODO: maybe in the future get closest field via levenshtein
			resolve(*field.value, scope, file_id);
			if (std::find_if(
				    struct_->fields.cbegin(),
				    struct_->fields.cend(),
				    [&field](AST::Struct::Field const& given_field) {
					    return given_field.name.value == field.name.value;
				    }
			    )
			    == struct_->fields.cend()) {
				std::stringstream subtitle_stream {};
				subtitle_stream
					<< "field '"
					<< field.name.value
					<< "' does not exist in struct '"
					<< struct_->name.value.name()
					<< "'";
				parsed_files.at(file_id).diagnostics.push_back(
					Diagnostic::error(
						"nonexistent struct field",
						subtitle_stream.str(),
						{Diagnostic::Sample(
							get_context(file_id),
							field.name.span,
							OutFmt::Color::Red
						)}
					)
				);
			}
		}

		// finally, we check whether any field is missing
		for (auto const& field : struct_->fields) {
			if (std::find_if(
				    struct_literal.fields.cbegin(),
				    struct_literal.fields.cend(),
				    [&field](AST::Expression::Atom::StructLiteral::Field const& given_field) {
					    return field.name.value == given_field.name.value;
				    }
			    )
			    == struct_literal.fields.cend()) {
				std::stringstream subtitle_stream {};
				subtitle_stream
					<< "field '"
					<< field.name.value
					<< "' exists in struct '"
					<< struct_->name.value.name()
					<< "', but it was not given a value in the struct literal";
				parsed_files.at(file_id).diagnostics.push_back(
					Diagnostic::error(
						"missing struct field",
						subtitle_stream.str(),
						{Diagnostic::Sample(get_context(file_id), span, OutFmt::Color::Red)}
					)
				);
			}
		}
	}
	// the rest of them do not need any kind of resolution yet
	return;
}

void Resolver::resolve(Spanned<AST::Expression>& expression, Scope const& scope, FileContext::ID file_id) {
	resolve(expression.value, expression.span, scope, file_id);
}

void Resolver::resolve(AST::Statement::Declare& declare, Scope& scope, FileContext::ID file_id) {
	if (declare.type.has_value()) resolve(declare.type.value(), scope, file_id);
	// resolve the value before the name, otherwise 'const a = a;' would not work
	if (declare.value.has_value()) resolve(declare.value.value(), scope, file_id);
	if (declare.name.value.id.has_value()) return;
	identify(declare.name.value);
	symbol_pool_.push_back(
		Symbol {declare.name.value.id.value()[0],
	                file_id,
	                declare.name.span,
	                declare.name.value.name(),
	                {},
	                register_type(
				declare.type.has_value() ? from_type(declare.type.value().value, file_id)
							 : TypeInfo::make_unknown(),
				declare.type.has_value() ? declare.type.value().span : declare.name.span,
				file_id,
				declare.name.value.id.value()[0]
			),
	                declare.mutable_.value,
	                false,
	                {}}
	);
	// intentionally replace (shadowing)
	scope.symbols.insert_or_assign(declare.name.value.name(), declare.name.value.id.value());
}

void Resolver::resolve(AST::Statement::Set& set, Scope& scope, FileContext::ID file_id) {
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
	case AST::Statement::Kind::Label:
	case AST::Statement::Kind::Goto:   return;
	case AST::Statement::Kind::Branch: resolve(statement.value.get_branch().condition, scope, file_id); return;
	case AST::Statement::Kind::If:     return;
	}
}

void Resolver::resolve(AST::Scope& ast_scope, Scope resolver_scope, FileContext::ID file_id) {
	for (auto& statement : ast_scope) { resolve(statement, resolver_scope, file_id); }
}

void Resolver::resolve(AST::Function& function, Scope scope, FileContext::ID file_id) {
	Scope child_scope {&scope, {}};
	for (auto& argument : function.arguments) {
		resolve(argument.type, scope, file_id);
		// intentionally replace (shadowing)
		child_scope.symbols.insert_or_assign(argument.name.value.name(), argument.name.value.id.value());
	}
	resolve(function.return_type, scope, file_id);
	if (function.body.has_value()) resolve(function.body.value(), child_scope, file_id);
}

void Resolver::resolve(AST::Struct& struct_, Scope const& scope, FileContext::ID file_id) {
	// as of now, we just resolve all fields within the struct
	for (auto& field : struct_.fields) {
		resolve(field.type, scope, file_id);
		// detect immediately recursive types (we allow pointers though, for now)
		if (field.type.value.is_atom()
		    && field.type.value.get_atom().is_named()
		    && field.type.value.get_atom().get_named().id.value().size() == 1
		    && field.type.value.get_atom().get_named().id.value().at(0)
		               == struct_.name.value.id.value().at(0)) {
			// TODO: move this somewhere else, probably during an IR pass, because we can't really detect
			// mutually recursive types this way, and we don't know how the other type will use this, etc
			std::cout << "recursive type detected !!" << std::endl;
			std::exit(0);
		}
	}
}

void Resolver::resolve(AST::Module& module, Scope scope, FileContext::ID file_id) {
	Scope child_scope {&scope, {}};

	// "hoist" module items by resolving those first
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
		} else if (std::holds_alternative<AST::Import>(value)) {
			auto& import = std::get<AST::Import>(value);
			resolve(import.name, scope, file_id, true);
			if (!import.name.value.id.has_value()) continue;
			// we need to make sure we only import exported symbols
			std::vector<AST::SymbolID> actual_ids {};
			for (AST::SymbolID id : import.name.value.id.value()) {
				if (symbol_pool_.at(id).exported) actual_ids.push_back(id);
			}
			import.name.value.id = actual_ids;
			if (actual_ids.empty()) {
				// TODO: show candidates
				parsed_files.at(file_id).diagnostics.push_back(
					Diagnostic::error(
						"import does not import any exported items",
						"none of the possible candidates for this import are exported",
						{Diagnostic::Sample(
							get_context(file_id),
							item.span,
							OutFmt::Color::Red
						)}
					)
				);
			}
			bool pushed_diagnostic = false;
			for (AST::SymbolID id : import.name.value.id.value()) {
				auto& imported_from = symbol_pool_.at(id).imported_from;
				if (symbol_pool_.at(id).file_id != file_id
				    && std::find(imported_from.cbegin(), imported_from.cend(), file_id)
				               == imported_from.cend()) {
					imported_from.push_back(file_id);
				} else if (!pushed_diagnostic) {
					// i don't think we should ever push this diagnostic more than once, because i
					// don't think there will be several ways to import the same name with different
					// IDs associated unless someone breaks aliases
					if (symbol_pool_.at(id).file_id == file_id)
						parsed_files.at(file_id).diagnostics.push_back(
							Diagnostic::warning(
								"redundant import",
								"this symbol was defined in this file",
								{Diagnostic::Sample(
									get_context(file_id),
									item.span,
									OutFmt::Color::Yellow
								)}
							)
						);
					else
						parsed_files.at(file_id).diagnostics.push_back(
							Diagnostic::warning(
								"redundant import",
								"this symbol has already been imported",
								{Diagnostic::Sample(
									get_context(file_id),
									item.span,
									OutFmt::Color::Yellow
								)}
							)
						);
					pushed_diagnostic = true;
				}
			}
		} else if (std::holds_alternative<AST::Struct>(value)) {
			auto& struct_ = std::get<AST::Struct>(value);
			child_scope.symbols.emplace(struct_.name.value.name(), struct_.name.value.id.value());
		}
	}

	// then actually resolve the bodies
	for (Spanned<AST::Module::Item>& item : module.body.items) {
		auto& value = std::get<AST::Module::InnerItem>(item.value);
		if (std::holds_alternative<AST::Function>(value))
			resolve(std::get<AST::Function>(value), child_scope, file_id);
		else if (std::holds_alternative<AST::Module>(value))
			resolve(std::get<AST::Module>(value), child_scope, file_id);
		else if (std::holds_alternative<AST::Struct>(value))
			resolve(std::get<AST::Struct>(value), child_scope, file_id);
	}
}

void Resolver::resolve_identifiers() {
	for (ParsedFile& file : parsed_files) { resolve(file.module, Scope {}, file.file_id); }
}

void Resolver::prune_named_partial_types() {
	for (TypeInfo& type : type_pool_)
		if (type.is_named_partial()) {
			// if we did not manage to resolve, we make a bottom
			if (!type.get_named_partial()->id.has_value()
			    || type.get_named_partial()->id.value().size() != 1)
				type = TypeInfo::make_bottom();
			// if we managed to resolve, we make a named known
			else type = TypeInfo::make_named_known(type.get_named_partial()->id.value().at(0));
		}
}
