#include "ir.hpp"
#include "resolver.hpp"

#include <variant>

void Resolver::identify(AST::Identifier& identifier) {
	assert(!identifier.id.has_value());
	identifier.id = {symbol_next()};
}

void Resolver::identify(AST::Module& module, bool exported, FileContext::ID file_id) {
	identify(module.name.value);
	symbol_pool_.push_back(
		Symbol {module.name.value.id.value()[0],
	                file_id,
	                module.name.span,
	                module.name.value.name(),
	                &module,
	                register_type(
				TypeInfo::make_module(),
				module.name.span,
				file_id,
				module.name.value.id.value()[0]
			),
	                false,
	                exported,
	                {}}
	);
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
	identify(struct_.name.value);
	symbol_pool_.push_back(
		Symbol {struct_.name.value.id.value()[0],
	                file_id,
	                struct_.name.span,
	                struct_.name.value.name(),
	                &struct_,
	                // TODO: register struct type
	                0,
	                false,
	                exported,
	                {}}
	);

	if (struct_.generic_declaration.has_value()) identify(struct_.generic_declaration.value(), file_id);
}

void Resolver::identify(AST::Trait& trait, bool exported, FileContext::ID file_id) {
	identify(trait.name.value);
	symbol_pool_.push_back(
		Symbol {trait.name.value.id.value()[0],
	                file_id,
	                trait.name.span,
	                trait.name.value.name(),
	                &trait,
	                // TODO: register trait type?
	                0,
	                false,
	                exported,
	                {}}
	);

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
	std::vector<std::tuple<std::optional<std::string>, TypeInfo::ID>> arguments {};
	arguments.reserve(function.arguments.size());

	for (auto& argument : function.arguments) {
		identify(argument.name.value);
		TypeInfo::ID type_id = register_type(
			from_type(argument.type.value, file_id),
			argument.type.span,
			file_id,
			argument.name.value.id.value()[0]
		);
		symbol_pool_.push_back(
			Symbol {argument.name.value.id.value()[0],
		                file_id,
		                argument.name.span,
		                argument.name.value.name(),
		                {},
		                type_id,
		                argument.mutable_,
		                false,
		                {}}
		);
		arguments.push_back(
			{argument.anonymous ? std::nullopt : std::optional {argument.name.value.name()}, type_id}
		);
	}

	std::vector<std::tuple<std::optional<std::string>, TypeInfo::ID>> generics {};
	if (function.generic_declaration.has_value()) {
		identify(function.generic_declaration.value(), file_id);
		for (auto& generic : function.generic_declaration.value().generics)
			generics.emplace_back(
				generic.anonymous ? std::nullopt : std::optional {generic.name.value.name()},
				get_single_symbol(generic.name.value).type
			);
	}

	TypeInfo::ID return_
		= register_type(from_type(function.return_type.value, file_id), function.return_type.span, file_id);

	identify(function.name.value);
	symbol_pool_.push_back(
		Symbol {function.name.value.id.value()[0],
	                file_id,
	                function.name.span,
	                function.name.value.name(),
	                &function,
	                register_type(
				TypeInfo::make_function(
					TypeInfo::Function {std::move(arguments), std::move(generics), return_}
				),
				function.name.span,
				file_id,
				function.name.value.id.value()[0]
			),
	                false,
	                exported,
	                {}}
	);
}

void Resolver::identify(AST::GenericDeclaration& generic_declaration, FileContext::ID file_id) {
	for (auto& generic : generic_declaration.generics) {
		identify(generic.name.value);
		TypeInfo::ID stub_type = register_type(
			TypeInfo::make_bottom(),
			generic.name.span,
			file_id,
			generic.name.value.id.value()[0]
		);
		symbol_pool_.push_back(
			Symbol {generic.name.value.id.value()[0],
		                file_id,
		                generic.name.span,
		                generic.name.value.name(),
		                Generic {},
		                stub_type,
		                false,
		                false,
		                {}}
		);
	}
}

void Resolver::identify_module_items() {
	for (ParsedFile& file : parsed_files) { identify(file.module, true, file.file_id); }
}

void Resolver::identify_built_in_operator(IR::BuiltInFunction function, Token::Symbol operator_, TypeInfo::ID type) {
	Span            span    = Span::zero();
	FileContext::ID file_id = FileContext::BUILT_IN_ID;

	AST::SymbolID id = symbol_next();

	symbol_pool_.push_back(
		Symbol {id, file_id, span, get_variant_name(operator_), function, type, false, true, {}}
	);

	type_symbol_mapping_.at(type) = id;
}

void Resolver::identify_built_in_unary_operator(
	IR::BuiltInFunction function,
	Token::Symbol       operator_,
	TypeInfo::ID        type,
	bool                generic
) {
	Span            span    = Span::zero();
	FileContext::ID file_id = FileContext::BUILT_IN_ID;

	std::vector<std::tuple<std::optional<std::string>, TypeInfo::ID>> generics {};
	if (generic) generics.emplace_back(std::nullopt, type);

	TypeInfo function_type
		= TypeInfo::make_function(TypeInfo::Function {{{std::nullopt, type}}, std::move(generics), type});
	identify_built_in_operator(function, operator_, register_type(std::move(function_type), span, file_id));
}

void Resolver::identify_built_in_binary_operator(
	IR::BuiltInFunction function,
	Token::Symbol       operator_,
	TypeInfo::ID        type,
	bool                generic
) {
	Span            span    = Span::zero();
	FileContext::ID file_id = FileContext::BUILT_IN_ID;

	std::vector<std::tuple<std::optional<std::string>, TypeInfo::ID>> generics {};
	if (generic) generics.emplace_back(std::nullopt, type);

	TypeInfo function_type = TypeInfo::make_function(
		TypeInfo::Function {
			{{std::nullopt, type}, {std::nullopt, type}},
			std::move(generics),
			type
        }
	);
	identify_built_in_operator(function, operator_, register_type(std::move(function_type), span, file_id));
}

void Resolver::identify_built_in_binary_comparison_operator(
	IR::BuiltInFunction function,
	Token::Symbol       operator_,
	TypeInfo::ID        type,
	bool                generic
) {
	Span            span    = Span::zero();
	FileContext::ID file_id = FileContext::BUILT_IN_ID;

	std::vector<std::tuple<std::optional<std::string>, TypeInfo::ID>> generics {};
	if (generic) generics.emplace_back(std::nullopt, type);

	TypeInfo::ID return_id     = register_type(TypeInfo::make_known_bool(), span, file_id);
	TypeInfo     function_type = TypeInfo::make_function(
                TypeInfo::Function {
			    {{std::nullopt, type}, {std::nullopt, type}},
                        std::move(generics),
                        return_id
        }
        );
	identify_built_in_operator(function, operator_, register_type(std::move(function_type), span, file_id));
}

Resolver::TypeInfo::ID Resolver::create_built_in_generic(std::string&& name, std::string&& trait_bound) {
	Span            span    = Span::zero();
	FileContext::ID file_id = FileContext::BUILT_IN_ID;

	AST::SymbolID trait_symbol = built_in_traits_.at(trait_bound).name.value.id.value().at(0);
	AST::SymbolID symbol_id    = symbol_next();

	TypeInfo::ID type_id = register_type(
		TypeInfo::make_generic(
			TypeInfo::Generic {symbol_id, {TypeInfo::Generic::TraitConstraint {trait_symbol, {}}}, {}}
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
		register_type(TypeInfo::make_known_char(), span, file_id)
	);
	identify_built_in_binary_comparison_operator(
		IR::BuiltInFunction::EqBools,
		Token::Symbol::EqEq,
		register_type(TypeInfo::make_known_bool(), span, file_id)
	);
	identify_built_in_binary_comparison_operator(
		IR::BuiltInFunction::NeChars,
		Token::Symbol::Ne,
		register_type(TypeInfo::make_known_char(), span, file_id)
	);
	identify_built_in_binary_comparison_operator(
		IR::BuiltInFunction::NeBools,
		Token::Symbol::Ne,
		register_type(TypeInfo::make_known_bool(), span, file_id)
	);
	// identify unary boolean negation
	identify_built_in_unary_operator(
		IR::BuiltInFunction::NegateBool,
		Token::Symbol::Bang,
		register_type(TypeInfo::make_known_bool(), span, file_id)
	);
}

void Resolver::push_built_in_trait(AST::Identifier const& identifier, std::vector<AST::SymbolID>&& constraints) {
	std::vector<TypeInfo::Generic::TraitConstraint> trait_constraints {};
	trait_constraints.reserve(constraints.size());
	std::transform(
		constraints.cbegin(),
		constraints.cend(),
		std::back_inserter(trait_constraints),
		[](AST::SymbolID id) { return TypeInfo::Generic::TraitConstraint {id, {}}; }
	);

	Span            span    = Span::zero();
	FileContext::ID file_id = FileContext::BUILT_IN_ID;
	symbol_pool_.push_back(
		Symbol {identifier.id.value().at(0),
	                file_id,
	                span,
	                identifier.name(),
	                &built_in_traits_.at(identifier.name()),
	                0,
	                false,
	                true,
	                {},
	                std::move(trait_constraints)}
	);
}

Spanned<AST::Identifier> Resolver::make_built_in_identifier(std::string name) {
	AST::SymbolID   id = symbol_next();
	AST::Identifier identifier({Span::zero(), std::move(name)});
	identifier.id = {id};
	return {Span::zero(), identifier};
}

void Resolver::identify_built_in_traits() {
	// we have to use this vector because the brace initializer loves copying
	std::vector<AST::Trait::Constraint> constraints {};

	auto int_id = make_built_in_identifier("int");
	built_in_traits_.insert_or_assign("int", AST::Trait {int_id, {}, {}});
	push_built_in_trait(int_id.value);

	auto uint_id = make_built_in_identifier("uint");
	constraints  = std::vector<AST::Trait::Constraint> {};
	constraints.push_back(AST::Trait::Constraint {int_id, {}});
	built_in_traits_.insert_or_assign("uint", AST::Trait {uint_id, {}, std::move(constraints)});
	push_built_in_trait(uint_id.value, {int_id.value.id.value()[0]});

	auto sint_id = make_built_in_identifier("sint");
	constraints  = std::vector<AST::Trait::Constraint> {};
	constraints.push_back(AST::Trait::Constraint {int_id, {}});
	built_in_traits_.insert_or_assign("sint", AST::Trait {sint_id, {}, std::move(constraints)});
	push_built_in_trait(sint_id.value, {int_id.value.id.value()[0]});

	auto float_id = make_built_in_identifier("float");
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
	case AST::Statement::Kind::If: return;
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
	case AST::Statement::Kind::If: return;
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
