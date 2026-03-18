#include "ir.hpp"
#include "resolver.hpp"

#include <variant>

void Resolver::identify(
	Spanned<AST::Name>&    name,
	decltype(Symbol::item) item,
	bool                   exported,
	TypeVar                type,
	FileContext::ID        file_id
) {
	assert(!name.value.id.has_value());
	name.value.id = register_symbol(
		Symbol {0,
	                0,
	                Span(0),
	                name.value.name,
	                std::move(item),
	                register_type(std::move(type), name.span, file_id),
	                false,
	                exported,
	                {}},
		name.span,
		file_id
	);
}

void Resolver::identify(AST::Module& module, bool exported, FileContext::ID file_id) {
	identify(module.name, &module, exported, TypeVar::make_module(), file_id);
	for (Spanned<AST::Module::Item>& item : module.body.items) {
		// TODO: disallow duplicate module (and trait?) names
		auto& value = std::get<AST::Module::InnerItem>(item.value);
		if (std::holds_alternative<AST::Function>(value))
			identify(std::get<AST::Function>(value), std::get<bool>(item.value), file_id);
		else if (std::holds_alternative<AST::Module>(value))
			identify(std::get<AST::Module>(value), std::get<bool>(item.value), file_id);
		else if (std::holds_alternative<AST::Struct>(value))
			identify(std::get<AST::Struct>(value), std::get<bool>(item.value), file_id);
		else if (std::holds_alternative<AST::Trait>(value))
			identify(std::get<AST::Trait>(value), std::get<bool>(item.value), file_id);
		else if (std::holds_alternative<AST::TraitImplementation>(value))
			identify(std::get<AST::TraitImplementation>(value), std::get<bool>(item.value), file_id);
	}
}

void Resolver::identify(AST::Struct& struct_, bool exported, FileContext::ID file_id) {
	identify(struct_.name, &struct_, exported, TypeVar::make_bottom(), file_id);

	if (struct_.generic_declaration.has_value()) identify(struct_.generic_declaration.value(), file_id);
}

void Resolver::identify(AST::Trait& trait, bool exported, FileContext::ID file_id) {
	identify(trait.name, &trait, exported, TypeVar::make_bottom(), file_id);

	if (trait.generic_declaration.has_value()) identify(trait.generic_declaration.value(), file_id);

	for (AST::Function& method : trait.methods) identify(method, exported, file_id);
}

void Resolver::identify(AST::TraitImplementation& trait_implementation, bool exported, FileContext::ID file_id) {
	if (trait_implementation.generic_declaration.has_value())
		identify(trait_implementation.generic_declaration.value(), file_id);

	// TODO: do we really want to make these methods have an ID? we probably instead want their ID to be linked with
	// the trait's method
	for (AST::Function& method : trait_implementation.methods) identify(method, exported, file_id);
}

void Resolver::identify(AST::Function& function, bool exported, FileContext::ID file_id) {
	std::vector<std::tuple<std::optional<std::string>, TypeVar::ID>> arguments {};
	arguments.reserve(function.arguments.size());

	for (auto& argument : function.arguments) {
		identify(argument.name, std::monostate {}, false, from_type(argument.type.value, file_id), file_id);

		arguments.push_back(
			{argument.anonymous ? std::nullopt : std::optional {argument.name.value.name},
		         get_single_symbol(argument.name.value).type}
		);
	}

	std::vector<std::tuple<std::optional<std::string>, TypeVar::ID>> generics {};
	if (function.generic_declaration.has_value()) {
		identify(function.generic_declaration.value(), file_id);
		for (auto& generic : function.generic_declaration.value().generics)
			generics.emplace_back(
				generic.anonymous ? std::nullopt : std::optional {generic.name.value.name},
				get_single_symbol(generic.name.value).type
			);
	}

	TypeVar::ID return_
		= register_type(from_type(function.return_type.value, file_id), function.return_type.span, file_id);

	// TODO: identify declarations

	// TODO: do something with these types above maybe?

	identify(function.name, &function, exported, TypeVar::make_bottom(), file_id);
}

void Resolver::identify(AST::GenericDeclaration& generic_declaration, FileContext::ID file_id) {
	for (auto& generic : generic_declaration.generics) {
		// TODO: maybe make this named too instead of relying on inference
		identify(generic.name, Generic {}, false, TypeVar::make_bottom(), file_id);
	}
}

void Resolver::identify_module_items() {
	for (ParsedFile& file : parsed_files) { identify(file.module, true, file.file_id); }
}

void Resolver::identify_built_in_operator(IR::BuiltInFunction function, Token::Symbol operator_, TypeVar::ID type) {
	/*
	Span            span    = Span::zero();
	FileContext::ID file_id = FileContext::BUILT_IN_ID;

	AST::SymbolID id = symbol_next();

	symbol_pool_.push_back(
	        Symbol {id, file_id, span, get_variant_name(operator_), function, type, false, true, {}}
	);
	*/
}

void Resolver::identify_built_in_unary_operator(
	IR::BuiltInFunction function,
	Token::Symbol       operator_,
	TypeVar::ID         type,
	bool                generic
) {
	Span            span    = Span::zero();
	FileContext::ID file_id = FileContext::BUILT_IN_ID;

	std::vector<std::tuple<std::optional<std::string>, TypeVar::ID>> generics {};
	if (generic) generics.emplace_back(std::nullopt, type);

	/*
	TypeVar function_type
	        = TypeVar::make_function(TypeVar::Function {{{std::nullopt, type}}, std::move(generics), type});
	identify_built_in_operator(function, operator_, register_type(std::move(function_type), span, file_id));
	*/
}

void Resolver::identify_built_in_binary_operator(
	IR::BuiltInFunction function,
	Token::Symbol       operator_,
	TypeVar::ID         type,
	bool                generic
) {
	Span            span    = Span::zero();
	FileContext::ID file_id = FileContext::BUILT_IN_ID;

	std::vector<std::tuple<std::optional<std::string>, TypeVar::ID>> generics {};
	if (generic) generics.emplace_back(std::nullopt, type);

	/*
	TypeVar function_type = TypeVar::make_function(
	        TypeVar::Function {
	                {{std::nullopt, type}, {std::nullopt, type}},
	                std::move(generics),
	                type
	}
	);
	identify_built_in_operator(function, operator_, register_type(std::move(function_type), span, file_id));
	*/
}

void Resolver::identify_built_in_binary_comparison_operator(
	IR::BuiltInFunction function,
	Token::Symbol       operator_,
	TypeVar::ID         type,
	bool                generic
) {
	Span            span    = Span::zero();
	FileContext::ID file_id = FileContext::BUILT_IN_ID;

	std::vector<std::tuple<std::optional<std::string>, TypeVar::ID>> generics {};
	if (generic) generics.emplace_back(std::nullopt, type);

	TypeVar::ID return_id = register_type(TypeVar::make_bool(), span, file_id);
	/*
	TypeVar     function_type = TypeVar::make_function(
	        TypeVar::Function {
	                    {{std::nullopt, type}, {std::nullopt, type}},
	                std::move(generics),
	                return_id
	}
	);
	identify_built_in_operator(function, operator_, register_type(std::move(function_type), span, file_id));
	*/
}

Resolver::TypeVar::ID Resolver::create_built_in_generic(std::string&& name, std::string&& trait_bound) {
	Span            span    = Span::zero();
	FileContext::ID file_id = FileContext::BUILT_IN_ID;

	AST::SymbolID trait_symbol = built_in_traits_.at(trait_bound).name.value.id.value();
	AST::SymbolID symbol_id    = symbol_next();

	/*
	TypeVar::ID type_id = register_type(
	        TypeVar::make_generic(
	                TypeVar::Generic {symbol_id, {TypeVar::Generic::TraitConstraint {trait_symbol, {}}}, {}}
	        ),
	        span,
	        file_id
	);

	symbol_pool_.push_back(
	        Symbol {symbol_id,
	                file_id,
	                span,
	                std::move(name),
	                Generic {},
	                type_id,
	                false,
	                false,
	                {},
	                symbol_pool_.at(trait_symbol).trait_constraints}
	);

	return type_id;
	*/
}

void Resolver::identify_built_in_operators() {
	Span            span    = Span::zero();
	FileContext::ID file_id = FileContext::BUILT_IN_ID;

	// identify plus, minus, star, div for integers
	// FIXME: clang-format goes crazy over this array
	std::vector<std::tuple<Token::Symbol, std::tuple<IR::BuiltInFunction, IR::BuiltInFunction>>>
		binary_integer_operators {
			{ Token::Symbol::Plus,{IR::BuiltInFunction::AddSIntegers, IR::BuiltInFunction::AddUIntegers}                                  },
			{Token::Symbol::Minus,
	                 {IR::BuiltInFunction::SubtractSIntegers, IR::BuiltInFunction::SubtractUIntegers}},
			{ Token::Symbol::Star,
	                 {IR::BuiltInFunction::MultiplySIntegers, IR::BuiltInFunction::MultiplyUIntegers}},
			{  Token::Symbol::Div,
	                 {IR::BuiltInFunction::DivideSIntegers, IR::BuiltInFunction::DivideUIntegers}    }
        };
	for (auto [operator_, functions] : binary_integer_operators) {
		auto [signed_, unsigned_] = functions;

		identify_built_in_binary_operator(unsigned_, operator_, create_built_in_generic("I", "uint"), true);
		identify_built_in_binary_operator(signed_, operator_, create_built_in_generic("I", "sint"), true);
	}
	// identify comparison for integers
	std::vector<std::tuple<Token::Symbol, std::tuple<IR::BuiltInFunction, IR::BuiltInFunction>>>
		binary_comparison_integer_operators {
			{Token::Symbol::EqEq,   {IR::BuiltInFunction::EqIntegers, IR::BuiltInFunction::EqIntegers}},
			{  Token::Symbol::Ne,   {IR::BuiltInFunction::NeIntegers, IR::BuiltInFunction::NeIntegers}},
			{  Token::Symbol::Gt, {IR::BuiltInFunction::GtSIntegers, IR::BuiltInFunction::GtUIntegers}},
			{  Token::Symbol::Ge, {IR::BuiltInFunction::GeSIntegers, IR::BuiltInFunction::GeUIntegers}},
			{  Token::Symbol::Lt, {IR::BuiltInFunction::LtSIntegers, IR::BuiltInFunction::LtUIntegers}},
			{  Token::Symbol::Le, {IR::BuiltInFunction::LeSIntegers, IR::BuiltInFunction::LeUIntegers}},
        };
	for (auto [operator_, functions] : binary_comparison_integer_operators) {
		auto [signed_, unsigned_] = functions;
		identify_built_in_binary_comparison_operator(
			unsigned_,
			operator_,
			create_built_in_generic("I", "uint"),
			true
		);
		identify_built_in_binary_comparison_operator(
			signed_,
			operator_,
			create_built_in_generic("I", "sint"),
			true
		);
	}
	// identify unary negation for integers
	identify_built_in_unary_operator(
		IR::BuiltInFunction::NegateSInteger,
		Token::Symbol::Minus,
		create_built_in_generic("I", "sint"),
		true
	);

	// identify plus, minus, star, div for floats
	std::vector<std::tuple<Token::Symbol, IR::BuiltInFunction>> binary_float_operators {
		{ Token::Symbol::Plus,      IR::BuiltInFunction::AddFloats},
		{Token::Symbol::Minus, IR::BuiltInFunction::SubtractFloats},
		{ Token::Symbol::Star, IR::BuiltInFunction::MultiplyFloats},
		{  Token::Symbol::Div,   IR::BuiltInFunction::DivideFloats}
	};
	for (auto [operator_, function] : binary_float_operators)
		identify_built_in_binary_operator(function, operator_, create_built_in_generic("F", "float"), true);
	// identify comparison for floats
	std::vector<std::tuple<Token::Symbol, IR::BuiltInFunction>> binary_comparison_float_operators {
		{Token::Symbol::EqEq, IR::BuiltInFunction::EqFloats},
		{  Token::Symbol::Ne, IR::BuiltInFunction::NeFloats},
		{  Token::Symbol::Gt, IR::BuiltInFunction::GtFloats},
		{  Token::Symbol::Ge, IR::BuiltInFunction::GeFloats},
		{  Token::Symbol::Lt, IR::BuiltInFunction::LtFloats},
		{  Token::Symbol::Le, IR::BuiltInFunction::LeFloats},
	};
	for (auto [operator_, function] : binary_comparison_float_operators)
		identify_built_in_binary_comparison_operator(
			function,
			operator_,
			create_built_in_generic("F", "float"),
			true
		);
	// identify unary negation for floats
	identify_built_in_unary_operator(
		IR::BuiltInFunction::NegateFloat,
		Token::Symbol::Minus,
		create_built_in_generic("F", "float"),
		true
	);
	// identify equality and inequality operator for char and bool
	identify_built_in_binary_comparison_operator(
		IR::BuiltInFunction::EqChars,
		Token::Symbol::EqEq,
		register_type(TypeVar::make_char(), span, file_id)
	);
	identify_built_in_binary_comparison_operator(
		IR::BuiltInFunction::EqBools,
		Token::Symbol::EqEq,
		register_type(TypeVar::make_bool(), span, file_id)
	);
	identify_built_in_binary_comparison_operator(
		IR::BuiltInFunction::NeChars,
		Token::Symbol::Ne,
		register_type(TypeVar::make_char(), span, file_id)
	);
	identify_built_in_binary_comparison_operator(
		IR::BuiltInFunction::NeBools,
		Token::Symbol::Ne,
		register_type(TypeVar::make_bool(), span, file_id)
	);
	// identify unary boolean negation
	identify_built_in_unary_operator(
		IR::BuiltInFunction::NegateBool,
		Token::Symbol::Bang,
		register_type(TypeVar::make_bool(), span, file_id)
	);
}

void Resolver::push_built_in_trait(AST::Name const& name, std::vector<AST::SymbolID>&& constraints) {
	/*
	std::vector<TypeVar::Generic::TraitConstraint> trait_constraints {};
	trait_constraints.reserve(constraints.size());
	std::transform(
	        constraints.cbegin(),
	        constraints.cend(),
	        std::back_inserter(trait_constraints),
	        [](AST::SymbolID id) { return TypeVar::Generic::TraitConstraint {id, {}}; }
	);

	Span            span    = Span::zero();
	FileContext::ID file_id = FileContext::BUILT_IN_ID;
	symbol_pool_.push_back(
	        Symbol {name.id.value(),
	                file_id,
	                span,
	                name.name,
	                &built_in_traits_.at(name.name),
	                0,
	                false,
	                true,
	                {},
	                std::move(trait_constraints)}
	);
	*/
}

Spanned<AST::Name> Resolver::make_built_in_name(std::string name_string) {
	AST::SymbolID id = symbol_next();
	AST::Name     name(std::move(name_string));
	name.id = {id};
	return {Span::zero(), name};
}

void Resolver::identify_built_in_traits() {
	Span span = Span::zero();

	// we have to use this vector because the brace initializer loves copying
	std::vector<AST::Trait::Constraint> constraints {};

	auto int_id = make_built_in_name("int");
	built_in_traits_.insert_or_assign("int", AST::Trait {int_id, {}, {}});
	push_built_in_trait(int_id.value);

	auto uint_id = make_built_in_name("uint");
	constraints  = std::vector<AST::Trait::Constraint> {};
	constraints.push_back(
		AST::Trait::Constraint {
			{span, AST::Identifier {span, int_id.value}}
        }
	);
	built_in_traits_.insert_or_assign("uint", AST::Trait {uint_id, {}, std::move(constraints)});
	push_built_in_trait(uint_id.value, {int_id.value.id.value()});

	auto sint_id = make_built_in_name("sint");
	constraints  = std::vector<AST::Trait::Constraint> {};
	constraints.push_back(
		AST::Trait::Constraint {
			{span, AST::Identifier {span, int_id.value}}
        }
	);
	built_in_traits_.insert_or_assign("sint", AST::Trait {sint_id, {}, std::move(constraints)});
	push_built_in_trait(sint_id.value, {int_id.value.id.value()});

	auto float_id = make_built_in_name("float");
	built_in_traits_.insert_or_assign("float", AST::Trait {float_id, {}, {}});
	push_built_in_trait(float_id.value);
}

void Resolver::identify_populate_labels(
	Spanned<AST::Statement>&                                             statement,
	std::unordered_map<std::string, Spanned<AST::Statement::Label::ID>>& labels,
	AST::Statement::Label::ID&                                           counter,
	FileContext::ID                                                      file_id
) {
	switch (statement.value.kind()) {
	case AST::Statement::Kind::Declare:
	case AST::Statement::Kind::Set:
	case AST::Statement::Kind::Expression:
	case AST::Statement::Kind::Return:     return;
	case AST::Statement::Kind::Scope:
		for (Spanned<AST::Statement>& substatement : statement.value.get_scope())
			identify_populate_labels(substatement, labels, counter, file_id);
		return;
	// we will deal with goto and branch later!
	case AST::Statement::Kind::Goto:
	case AST::Statement::Kind::Branch: return;
	case AST::Statement::Kind::Label:  break;
	// more complex control flow is desugared later
	case AST::Statement::Kind::If:
	case AST::Statement::Kind::While:
	case AST::Statement::Kind::Break:
	case AST::Statement::Kind::Continue: return;
	}

	if (statement.value.is_label()) {
		// skip automatically generated labels with a valid ID
		if (statement.value.get_label().id.has_value()) return;
		std::string const& name = statement.value.get_label().name;
		if (labels.contains(name)) {
			parsed_files.at(file_id).diagnostics.push_back(
				Diagnostic::error(
					"label redefinition",
					"labels must have unique names",
					{Diagnostic::Sample(
						 get_context(file_id),
						 {Diagnostic::Sample::Label(
							 labels.at(name).span,
							 "defined here",
							 OutFmt::Color::Cyan
						 )}
					 ),
			                 Diagnostic::Sample(
						 get_context(file_id),
						 {Diagnostic::Sample::Label(
							 statement.span,
							 "redefined here",
							 OutFmt::Color::Red
						 )}
					 )}
				)
			);
			return;
		}
		AST::Statement::Label::ID id = counter++;
		labels.emplace(name, Spanned<AST::Statement::Label::ID> {statement.span, id});
		statement.value.get_label().id = id;
		return;
	}
}

void Resolver::identify_add_unknown_label_diagnostic(Span span, FileContext::ID file_id) {
	parsed_files.at(file_id).diagnostics.push_back(
		Diagnostic::error(
			"unknown label",
			{
				Diagnostic::Sample(get_context(file_id), span, OutFmt::Color::Red),
			}
		)
	);
}

void Resolver::identify_labels(
	Spanned<AST::Statement>&                                                   statement,
	std::unordered_map<std::string, Spanned<AST::Statement::Label::ID>> const& labels,
	FileContext::ID                                                            file_id
) {
	switch (statement.value.kind()) {
	case AST::Statement::Kind::Declare:
	case AST::Statement::Kind::Set:
	case AST::Statement::Kind::Expression:
	case AST::Statement::Kind::Return:     return;
	case AST::Statement::Kind::Scope:
		for (Spanned<AST::Statement>& substatement : statement.value.get_scope())
			identify_labels(substatement, labels, file_id);
		return;
	// labels are already identified
	case AST::Statement::Kind::Label:  return;
	case AST::Statement::Kind::Goto:
	case AST::Statement::Kind::Branch: break;
	// more complex control flow is desugared later
	case AST::Statement::Kind::If:
	case AST::Statement::Kind::While:
	case AST::Statement::Kind::Break:
	case AST::Statement::Kind::Continue: return;
	}

	if (statement.value.is_goto()) {
		// skip automatically generated gotos with a valid ID
		if (statement.value.get_goto().destination_id.has_value()) return;
		std::string const& destination = statement.value.get_goto().destination;
		if (!labels.contains(destination)) {
			identify_add_unknown_label_diagnostic(statement.span, file_id);
			return;
		}
		AST::Statement::Label::ID id = labels.at(destination).value;

		statement.value.get_goto().destination_id = id;
		return;
	} else if (statement.value.is_branch()) {
		// skip automatically generated branches with valid IDs
		if (statement.value.get_branch().true_.value.destination_id.has_value()) return;

		AST::Statement::Branch& branch = statement.value.get_branch();

		// resolve the true destination
		std::string const& true_destination = branch.true_.value.destination;
		if (!labels.contains(true_destination)) {
			identify_add_unknown_label_diagnostic(branch.true_.span, file_id);
			return;
		}
		branch.true_.value.destination_id = labels.at(true_destination).value;

		// resolve the false destination if it exists
		if (branch.false_.has_value()) {
			std::string const& false_destination = branch.false_.value().value.destination;
			if (!labels.contains(false_destination)) {
				identify_add_unknown_label_diagnostic(branch.false_.value().span, file_id);
				return;
			}
			branch.false_.value().value.destination_id = labels.at(false_destination).value;
		}
	}
}

void Resolver::identify_labels(AST::Function& function, FileContext::ID file_id) {
	if (!function.body.has_value()) return;
	std::unordered_map<std::string, Spanned<AST::Statement::Label::ID>> labels {};

	for (Spanned<AST::Statement>& statement : function.body.value()) {
		identify_populate_labels(statement, labels, function.label_counter, file_id);
	}

	// now that we have the label table, we can identify labels
	for (Spanned<AST::Statement>& statement : function.body.value()) {
		identify_labels(statement, labels, file_id);
	}
}

void Resolver::identify_labels(AST::Module& module, FileContext::ID file_id) {
	for (Spanned<AST::Module::Item>& item : module.body.items) {
		auto& value = std::get<AST::Module::InnerItem>(item.value);
		if (std::holds_alternative<AST::Function>(value))
			identify_labels(std::get<AST::Function>(value), file_id);
		else if (std::holds_alternative<AST::Module>(value))
			identify_labels(std::get<AST::Module>(value), file_id);
	}
}

void Resolver::identify_labels() {
	for (ParsedFile& file : parsed_files) { identify_labels(file.module, file.file_id); }
}
