#include "levenshtein.hpp"
#include "resolver.hpp"

#include <sstream>

bool Resolver::Symbol::is_visible(FileContext::ID other_id) const {
	// if we're in the same file, it is trivially visible
	if (file_id == other_id) return true;
	// if it has been imported from the other file, it is visible
	if (std::find(imported_from.cbegin(), imported_from.cend(), other_id) != imported_from.cend()) return true;
	// otherwise, it is never visible
	return false;
}

AST::SymbolID Resolver::register_symbol(Symbol symbol, Span span, FileContext::ID file_id) {
	symbol.id      = symbol_next();
	symbol.span    = span;
	symbol.file_id = file_id;
	symbol_pool_.push_back(std::move(symbol));
	return symbol.id;
}

Resolver::NameVar::ID Resolver::register_name(NameVar name, Span span, FileContext::ID file_id) {
	// span and file_id are unused, but they could prove useful later on, so we ask for them
	NameVar::ID id = name_next();
	name_pool_.push_back(std::move(name));
	return id;
}

Resolver::TypeVar::ID Resolver::register_type(TypeVar type, Span span, FileContext::ID file_id) {
	TypeVar::ID id = type_next();
	type_pool_.push_back(std::move(type));
	type_span_pool_.push_back({span, file_id});
	return id;
}

#define CLOSEST_THRESHOLD 3
#define CLOSEST_MAX       5

void Resolver::add_unknown_symbol_diagnostic(
	std::string_view                symbol,
	Span                            span,
	std::vector<std::string> const& possible_symbols,
	std::string_view                scope_type,
	FileContext::ID                 file_id,
	bool                            add_import_suggestion
) {
	std::string       title = std::format("unknown symbol `{}`", symbol);
	std::stringstream subtitle_stream {};
	subtitle_stream << "could not find any symbol with that name in the " << scope_type << " scope";

	std::vector<std::string_view> closest_symbols
		= closest(symbol, possible_symbols, CLOSEST_THRESHOLD, CLOSEST_MAX);

	if (!closest_symbols.empty()) {
		subtitle_stream << " (did you perhaps mean ";
		for (size_t i = 0; i < closest_symbols.size(); ++i) {
			subtitle_stream << '`' << closest_symbols[i] << '`';
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

AST::Name
Resolver::create_name(std::string_view name, TypeVar type, bool mutable_, Span span, FileContext::ID file_id) {
	AST::SymbolID id = register_symbol(
		Symbol {0,
	                0,
	                Span(0),
	                std::string(name),
	                std::monostate {},
	                register_type(type, span, file_id),
	                mutable_,
	                false,
	                {}},
		span,
		file_id
	);
	AST::Name var {std::string(name)};
	var.id = {id};
	return var;
}

Resolver::TypeVar Resolver::from_type(AST::Type::Atom const& atom, FileContext::ID file_id) {
	switch (atom.kind()) {
	case AST::Type::Atom::Kind::Float: return TypeVar::make_float(TypeVar::KnownFloat {atom.get_float().width});
	case AST::Type::Atom::Kind::Void:  return TypeVar::make_void();
	case AST::Type::Atom::Kind::Char:  return TypeVar::make_char();
	case AST::Type::Atom::Kind::Bool:  return TypeVar::make_bool();
	case AST::Type::Atom::Kind::Named: {
		// FIXME: this does not resolve the identifier!!! we need to create all appropriate branches
		NameVar::ID name = register_name(NameVar(), atom.get_named().name.span, file_id);
		return TypeVar::make_named(name);
	}
	case AST::Type::Atom::Kind::Inferred: return TypeVar::make_unknown();
	case AST::Type::Atom::Kind::Integer:  {
		// for integers, we need to determine how much information we know
		AST::Type::Atom::Integer const& integer = atom.get_integer();
		if (integer.width_type() != AST::Type::Atom::Integer::WidthType::Any)
			return TypeVar::make_integer(TypeVar::KnownInteger {integer});
		else return TypeVar::make_partial_integer(TypeVar::PartialInteger {integer, true});
	}
	}
}

Resolver::TypeVar Resolver::from_type(AST::Type const& type, FileContext::ID file_id) {
	switch (type.kind()) {
	case AST::Type::Kind::Atom: return from_type(type.get_atom(), file_id);
	case AST::Type::Kind::Pointer:
		return TypeVar::make_pointer(
			register_type(
				from_type(type.get_pointer().type->value, file_id),
				type.get_pointer().type->span,
				file_id
			),
			type.get_pointer().mutable_
		);
	}
}

IR::Type Resolver::reconstruct_type(TypeVar::ID type_id, TypeVar::ID type_origin, bool allow_functions) {
	assert(false && "TODO: type reconstruction");
}

IR::Type Resolver::reconstruct_type(TypeVar::ID, bool allow_functions) {
	assert(false && "TODO: type reconstruction");
}

// TODO: for each statement, infer type vars and create Eq & Sub constraints.

void Resolver::resolve_root(
	AST::Identifier& identifier,
	Span             span,
	Scope const&     scope,
	FileContext::ID  file_id,
	bool             include_unimported
) {
	// TODO: filter only imported items if the identifier only has a root!

	// do not try to resolve if we already reached this
	if (!identifier.root().is_unreached()) return;

	AST::Identifier::Segment& root = identifier.root();

	// if it's not absolute, we determine it from the scope
	if (!identifier.absolute()) {
		// happy path for built-in traits
		if (built_in_traits_.contains(root.name)) {
			root.candidates = {built_in_traits_.at(root.name).name.value.id.value()};
			return;
		}

		Scope const* traversing_scope = &scope;
		while (traversing_scope != nullptr) {
			if (traversing_scope->symbols.contains(root.name)) {
				root.candidates = traversing_scope->symbols.at(root.name);
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

		add_unknown_symbol_diagnostic(root.name, span, symbol_vector, "current", file_id);

		root.candidates = std::vector<AST::SymbolID> {};  // ensure to mark candidates as an empty array, thus
		                                                  // marking it as reached!
		return;
	}

	// if it is absolute, the root must exist in the module table
	if (!module_table_.contains(root.name)) {
		std::vector<std::string> modules {};
		modules.reserve(module_table_.size());
		std::transform(
			module_table_.cbegin(),
			module_table_.cend(),
			std::back_inserter(modules),
			[](auto const& v) { return v.first; }
		);

		add_unknown_symbol_diagnostic(root.name, root.span, modules, "global", file_id);

		root.candidates = std::vector<AST::SymbolID> {};  // ensure to mark candidates as an empty array, thus
		                                                  // marking it as reached!
		return;
	}

	root.candidates = {module_table_.at(root.name)->name.value.id.value()};
}

void Resolver::resolve_next(
	AST::Identifier& identifier,
	Span             span,
	Scope const&     scope,
	FileContext::ID  file_id,
	bool             include_unimported
) {
	// TODO: apply generic list!!

	if (!identifier.can_be_name_resolved()) {
		std::cout
			<< "warning: tried to keep resolving an identifier which cannot be name resolved further"
			<< std::endl;
		return;
	}

	// we have our new relative root and our unreached segment
	AST::Identifier::Segment& root      = identifier.last_decided_segment_before_unreached();
	AST::Identifier::Segment& unreached = identifier.first_unreached_segment();

	Symbol& root_symbol = symbol_pool_.at(root.id());

	std::unordered_map<std::string, std::vector<AST::SymbolID>> items {};

	// now, we get all items within our root
	if (std::holds_alternative<AST::Module*>(root_symbol.item)) {
		auto& module = *std::get<AST::Module*>(root_symbol.item);
		for (auto const& item : module.body.items) {
			auto const& value = std::get<AST::Module::InnerItem>(item.value);
			if (std::holds_alternative<AST::Import>(value)
			    || std::holds_alternative<AST::TraitImplementation>(value))
				continue;

			auto& name = AST::Module::get_actual_name(value);
			if (items.contains(name.name)) items.at(name.name).push_back(name.id.value());
			else items.insert_or_assign(name.name, std::vector {name.id.value()});
		}
	} else if (std::holds_alternative<AST::Function*>(root_symbol.item)) {
		assert(false && "TODO: diagnostic for trying to access a function's static members");
		unreached.candidates = std::vector<AST::SymbolID> {};  // ensure to mark candidates as an empty array,
		                                                       // thus marking it as reached!
		return;
	} else if (std::holds_alternative<AST::Struct*>(root_symbol.item)) {
		assert(false && "TODO: struct static members");
		unreached.candidates = std::vector<AST::SymbolID> {};  // ensure to mark candidates as an empty array,
		                                                       // thus marking it as reached!
		return;
	} else if (std::holds_alternative<AST::Trait*>(root_symbol.item)) {
		auto& trait = *std::get<AST::Trait*>(root_symbol.item);
		for (auto const& method : trait.methods) {
			auto& name = method.name.value;
			if (items.contains(name.name)) items.at(name.name).push_back(name.id.value());
			else items.insert_or_assign(name.name, std::vector {name.id.value()});
		}
	} else {
		assert(false && "TODO: diagnostic for trying to access a static member of a variable");
		unreached.candidates = std::vector<AST::SymbolID> {};  // ensure to mark candidates as an empty array,
		                                                       // thus marking it as reached!
		return;
	}

	bool add_import_suggestion = false;

	// if the unreached segment's name exists within those items, we set the correct candidates
	if (items.contains(unreached.name)) {
		// if we include unimported or we're not at the tip, we just set the candidates directly
		if (include_unimported || !identifier.last_is_only_unreached()) {
			unreached.candidates = items.at(unreached.name);
			return;
		}

		// otherwise, we determine the candidates by which are imported
		std::vector<AST::SymbolID> candidates {};
		std::copy_if(
			items.at(unreached.name).cbegin(),
			items.at(unreached.name).cend(),
			std::back_inserter(candidates),
			[this, file_id](AST::SymbolID candidate) {
				return symbol_pool_.at(candidate).is_visible(file_id);
			}
		);

		// if some candidates are imported, we set them
		if (!candidates.empty()) {
			unreached.candidates = items.at(unreached.name);
			return;
		}

		// if no candidates are imported, we must throw a diagnostic
		add_import_suggestion = true;
	}

	// otherwise, we throw a diagnostic
	std::vector<std::string> possibilities {};
	// push only possibilities that could've been visible
	for (auto& [possibility, id] : items)
		if (std::any_of(id.cbegin(), id.cend(), [this, file_id](AST::SymbolID candidate) {
			    return symbol_pool_.at(candidate).is_visible(file_id);
		    }))
			possibilities.push_back(std::move(possibility));

	add_unknown_symbol_diagnostic(
		unreached.name,
		unreached.span,
		possibilities,
		"specified",
		file_id,
		add_import_suggestion
	);

	unreached.candidates = std::vector<AST::SymbolID> {};  // ensure to mark candidates as an empty array, thus
	                                                       // marking it as reached!
	return;
}

void Resolver::resolve(
	AST::Identifier& identifier,
	Span             span,
	FileContext::ID  file_id,
	Scope const&     scope,
	bool             include_unimported
) {
	resolve_root(identifier, span, scope, file_id, include_unimported);
	while (identifier.can_be_name_resolved()) resolve_next(identifier, span, scope, file_id, include_unimported);
	// if it is middle undecided, it is an error
	// TODO: diagnostic?
	if (identifier.has_middle_undecided())
		identifier.last_undecided_segment().candidates = std::vector<AST::SymbolID> {};
	assert(!identifier.has_middle_undecided() && "unsupported middle undecided");
}

void Resolver::resolve(
	Spanned<AST::Identifier>& identifier,
	FileContext::ID           file_id,
	Scope const&              scope,
	bool                      include_unimported
) {
	return resolve(identifier.value, identifier.span, file_id, scope, include_unimported);
}

Resolver::TypeVar::ID
Resolver::infer(AST::Identifier& identifier, Span span, FileContext::ID file_id, Scope const& scope, InferCtx& ctx) {
	resolve(identifier, span, file_id, scope);
	if (!identifier.has_at_least_one_id()) return register_type(TypeVar::make_bottom(), span, file_id);
	auto type = register_type(TypeVar::make_unknown(), span, file_id);
	if (identifier.is_decided()) {
		auto actual_type = get_single_symbol(identifier.id()).type;

		type_pool_.at(type_substs_.get(type)) = type_pool_.at(type_substs_.get(actual_type));
		type_substs_.merge(type, actual_type);
	}
	// TODO: what if it's not decided?
	return type;
}

Resolver::TypeVar::ID
Resolver::infer(AST::Expression& expression, Span span, FileContext::ID file_id, Scope const& scope, InferCtx& ctx) {
	switch (expression.kind()) {
	case AST::Expression::Kind::Atom: {
		auto& atom = expression.get_atom();
		switch (atom.kind()) {
		case AST::Expression::Atom::Kind::Identifier: {
			expression.type = infer(atom.get_identifier(), span, file_id, scope, ctx);
		} break;
		case AST::Expression::Atom::Kind::NumberLiteral: {
			auto& literal   = atom.get_number_literal();
			expression.type = register_type(
				literal.is_float()
					? TypeVar::make_partial_float()
					: TypeVar::make_partial_integer({AST::Type::Atom::Integer::any(false), false}),
				span,
				file_id
			);
		} break;
		case AST::Expression::Atom::Kind::StringLiteral: {
			assert(false && "string literals are not yet supported");
		} break;
		case AST::Expression::Atom::Kind::CharLiteral: {
			expression.type = register_type(TypeVar::make_char(), span, file_id);
		} break;
		case AST::Expression::Atom::Kind::BoolLiteral: {
			expression.type = register_type(TypeVar::make_bool(), span, file_id);
		} break;
		case AST::Expression::Atom::Kind::StructLiteral: {
			// TODO: struct literals
			expression.type = 0;
		} break;
		case AST::Expression::Atom::Kind::Expression: {
			auto& subexpr   = *atom.get_expression();
			expression.type = infer(subexpr, span, file_id, scope, ctx);
		} break;
		}
	} break;
	case AST::Expression::Kind::UnaryOperation:
	case AST::Expression::Kind::BinaryOperation: {
		bool is_unary = expression.is_unary_operation();

		Token::Symbol operator_ = is_unary ? expression.get_unary_operation().operation
		                                   : expression.get_binary_operation().operation;

		if (is_unary && operator_ == Token::Symbol::Star) {
			auto& operation = expression.get_unary_operation();
			// *(...: $ptr): $expr
			TypeVar::ID ptr_type  = infer(*operation.operand, file_id, scope, ctx);
			TypeVar::ID expr_type = register_type(TypeVar::make_unknown(), span, file_id);
			// $expr <- *(...: $ptr)
			ctx.program.push_back(Constraint::deref(expr_type, ptr_type));
			expression.type = expr_type;
			break;
		}

		// TODO: get operator span
		Span operator_span = span;

		// TODO: get all operator candidates, then create a function call and resolve that
		expression.type = 0;
	} break;
	case AST::Expression::Kind::AddressOperation: {
		auto& operation = expression.get_address_operation();
		// &<const|mut> (...: $addr)
		TypeVar::ID addr_type = infer(*operation.operand, file_id, scope, ctx);
		// $expr = &<const|mut> $addr
		TypeVar::ID expr_type
			= register_type(TypeVar::make_pointer(addr_type, operation.mutable_), span, file_id);
		expression.type = expr_type;
	} break;
	case AST::Expression::Kind::FunctionCall: {
		// TODO: function calls
		expression.type = 0;
	} break;
	case AST::Expression::Kind::MemberAccess: {
		// TODO: member access
		expression.type = 0;
	} break;
	case AST::Expression::Kind::If: std::unreachable();
	}

	return expression.type.value();
}

Resolver::TypeVar::ID
Resolver::infer(Spanned<AST::Expression>& expression, FileContext::ID file_id, Scope const& scope, InferCtx& ctx) {
	return infer(expression.value, expression.span, file_id, scope, ctx);
}

void Resolver::infer(Spanned<AST::Statement>& statement, FileContext::ID file_id, Scope& scope, InferCtx& ctx) {
	switch (statement.value.kind()) {
	case AST::Statement::Kind::Declare: {
		auto& declare = statement.value.get_declare();
		// add to scope!
		scope.symbols.insert_or_assign(declare.name.value.name, std::vector {declare.name.value.id.value()});
		if (!declare.value.has_value()) return;
		// const a: $var = ...: $val
		TypeVar::ID var_type = get_single_symbol(declare.name.value).type;
		TypeVar::ID val_type = infer(declare.value.value(), file_id, scope, ctx);
		// $var <- $val
		ctx.program.push_back(Constraint::type_assg(var_type, val_type));
		return;
	}
	case AST::Statement::Kind::Set: {
		auto& set = statement.value.get_set();
		if (!set.lhs.value.can_be_lhs()) return;  // skip invalid LHS
		// ...: $lhs = ...: $rhs
		TypeVar::ID lhs_type = infer(set.lhs, file_id, scope, ctx);
		TypeVar::ID rhs_type = infer(set.rhs, file_id, scope, ctx);
		// $lhs <- $rhs
		ctx.program.push_back(Constraint::type_assg(lhs_type, rhs_type));
		return;
	}
	case AST::Statement::Kind::Expression: {
		infer(statement.value.get_expression(), statement.span, file_id, scope, ctx);
		return;
	}
	case AST::Statement::Kind::Return: {
		TypeVar::ID return_value
			= statement.value.get_return()
		                  .value
		                  .transform(
					  [this, file_id, &scope, &ctx](
						  Spanned<AST::Expression>& expression
					  ) -> TypeVar::ID { return infer(expression, file_id, scope, ctx); }
				  )
		                  .or_else([this, &statement, file_id]() {
					  return std::optional {
						  register_type(TypeVar::make_void(), statement.span, file_id)
					  };
				  })
		                  .value();
		// TODO: add constraint
		return;
	}
	case AST::Statement::Kind::Scope: {
		for (auto& sub_statement : statement.value.get_scope()) infer(sub_statement, file_id, scope, ctx);
		return;
	}
	case AST::Statement::Kind::Label:
	case AST::Statement::Kind::Goto:   return;
	case AST::Statement::Kind::Branch: {
		auto& branch = statement.value.get_branch();
		// branch (...: $cond) 't 'f;
		TypeVar::ID cond_type = infer(branch.condition, file_id, scope, ctx);
		// TODO: signal that this is a built-in requirement, maybe through the constraint
		TypeVar::ID bool_type = register_type(TypeVar::make_bool(), branch.condition.span, file_id);
		// bool <- $cond
		ctx.program.push_back(Constraint::type_assg(bool_type, cond_type));
		return;
	}
	case AST::Statement::Kind::If:
	case AST::Statement::Kind::While:
	case AST::Statement::Kind::Break:
	case AST::Statement::Kind::Continue: std::unreachable();
	}
}

void Resolver::infer(
	AST::TraitImplementation& trait_implementation,
	FileContext::ID           file_id,
	Scope                     parent,
	InferCtx&                 ctx
) {
	Scope scope {&parent, {}};
	for (auto& method : trait_implementation.methods) infer(method, file_id, scope, ctx);
}

void Resolver::infer(AST::Trait& trait, FileContext::ID file_id, Scope parent, InferCtx& ctx) {
	Scope scope {&parent, {}};
	for (auto& method : trait.methods) infer(method, file_id, scope, ctx);
}

void Resolver::infer(AST::Struct& struct_, FileContext::ID file_id, Scope parent, InferCtx& ctx) {
	Scope scope {&parent, {}};
	// TODO: struct methods
	// TODO: register field types in identify
}

void Resolver::infer(AST::Function& function, FileContext::ID file_id, Scope parent, InferCtx& ctx) {
	Scope scope {&parent, {}};
	// TODO: ensure function signature is sound?

	if (!function.body.has_value()) return;

	for (auto& statement : function.body.value()) infer(statement, file_id, scope, ctx);
}

void Resolver::infer(AST::Module& module, FileContext::ID file_id, Scope parent, InferCtx& ctx) {
	Scope scope {&parent, {}};
	// first add all names
	for (Spanned<AST::Module::Item>& item : module.body.items) {
		auto& value = std::get<AST::Module::InnerItem>(item.value);
		if (std::holds_alternative<AST::Function>(value)) {
			// TODO: deal with specializations
			auto& function = std::get<AST::Function>(value);
			if (scope.symbols.contains(function.name.value.name))
				scope.symbols.at(function.name.value.name).push_back(function.name.value.id.value());
			else
				scope.symbols.emplace(
					function.name.value.name,
					std::vector {function.name.value.id.value()}
				);
		} else if (std::holds_alternative<AST::Module>(value)) {
			auto& submodule = std::get<AST::Module>(value);
			scope.symbols.emplace(submodule.name.value.name, std::vector {submodule.name.value.id.value()});
		} else if (std::holds_alternative<AST::Alias>(value)) {
			auto& alias = std::get<AST::Alias>(value);
			resolve(alias.value, file_id, scope);
			if (!alias.value.value.has_at_least_one_id()) continue;
			scope.symbols.emplace(alias.name.value.name, alias.value.value.ids());
		} else if (std::holds_alternative<AST::Import>(value)) {
			auto& import = std::get<AST::Import>(value);
			resolve(import.name, file_id, scope, true);
			if (!import.name.value.has_at_least_one_id()) continue;
			// we need to make sure we only import exported symbols
			std::vector<AST::SymbolID> actual_ids {};
			for (AST::SymbolID id : import.name.value.ids()) {
				if (symbol_pool_.at(id).exported) actual_ids.push_back(id);
			}
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
				import.name.value.force_ids({});
				continue;
			}
			import.name.value.force_ids(std::move(actual_ids));
			bool pushed_diagnostic = false;
			for (AST::SymbolID id : import.name.value.ids()) {
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
			// TODO: deal with specializations
			auto& struct_ = std::get<AST::Struct>(value);
			scope.symbols.emplace(struct_.name.value.name, std::vector {struct_.name.value.id.value()});
		} else if (std::holds_alternative<AST::Trait>(value)) {
			auto& trait = std::get<AST::Trait>(value);
			scope.symbols.emplace(trait.name.value.name, std::vector {trait.name.value.id.value()});
		}
	}

	for (Spanned<AST::Module::Item>& item : module.body.items) {
		auto& value = std::get<AST::Module::InnerItem>(item.value);
		if (std::holds_alternative<AST::Function>(value))
			infer(std::get<AST::Function>(value), file_id, scope, ctx);
		else if (std::holds_alternative<AST::Module>(value))
			infer(std::get<AST::Module>(value), file_id, scope, ctx);
		else if (std::holds_alternative<AST::Struct>(value))
			infer(std::get<AST::Struct>(value), file_id, scope, ctx);
		else if (std::holds_alternative<AST::Trait>(value))
			infer(std::get<AST::Trait>(value), file_id, scope, ctx);
		else if (std::holds_alternative<AST::TraitImplementation>(value))
			infer(std::get<AST::Trait>(value), file_id, scope, ctx);
	}
}

void Resolver::infer() {
	InferCtx ctx {};
	for (ParsedFile& file : parsed_files) infer(file.module, file.file_id, {}, ctx);

	for (auto const& constraint : ctx.program) switch (constraint.kind()) {
		case Constraint::Kind::NameAssg: {
			auto const& name_assg = constraint.get_name_assg();
			debug_print_name(std::cout, name_assg.to) << " <- ";
			debug_print_name(std::cout, name_assg.from) << '\n';
		} break;
		case Constraint::Kind::TypeAssg: {
			auto const& type_assg = constraint.get_type_assg();
			debug_print_type(std::cout, type_assg.to) << " <- ";
			debug_print_type(std::cout, type_assg.from) << '\n';
		} break;
		case Constraint::Kind::Deref: {
			auto const& deref = constraint.get_deref();
			debug_print_type(std::cout, deref.to) << " <- *";
			debug_print_type(std::cout, deref.from) << '\n';
		} break;
		}
}
