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
		auto& value = std::get<AST::Module::InnerItem>(item.value);
		if (std::holds_alternative<AST::Function>(value))
			identify(std::get<AST::Function>(value), std::get<bool>(item.value), file_id);
		else if (std::holds_alternative<AST::Module>(value))
			identify(std::get<AST::Module>(value), std::get<bool>(item.value), file_id);
		else if (std::holds_alternative<AST::Struct>(value))
			identify(std::get<AST::Struct>(value), std::get<bool>(item.value), file_id);
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
				TypeInfo::make_function(TypeInfo::Function {std::move(arguments), return_}),
				function.name.span,
				file_id,
				function.name.value.id.value()[0]
			),
	                false,
	                exported,
	                {}}
	);
}

void Resolver::identify_module_items() {
	for (ParsedFile& file : parsed_files) { identify(file.module, true, file.file_id); }
}

void Resolver::identify_built_in_operator(IR::BuiltInFunction function, Token::Symbol operator_, TypeInfo&& type) {
	Span            span    = Span::zero();
	FileContext::ID file_id = FileContext::BUILT_IN_ID;

	AST::SymbolID id = symbol_next();

	symbol_pool_.push_back(
		Symbol {id,
	                file_id,
	                span,
	                get_variant_name(operator_),
	                function,
	                register_type(std::move(type), span, file_id, id),
	                false,
	                true,
	                {}}
	);
}

void Resolver::identify_built_in_unary_operator(
	IR::BuiltInFunction function,
	Token::Symbol       operator_,
	TypeInfo&&          type
) {
	Span            span    = Span::zero();
	FileContext::ID file_id = FileContext::BUILT_IN_ID;

	TypeInfo::ID type_id       = register_type(std::move(type), span, file_id);
	TypeInfo     function_type = TypeInfo::make_function(TypeInfo::Function {{{std::nullopt, type_id}}, type_id});
	identify_built_in_operator(function, operator_, std::move(function_type));
}

void Resolver::identify_built_in_binary_operator(
	IR::BuiltInFunction function,
	Token::Symbol       operator_,
	TypeInfo&&          type
) {
	Span            span    = Span::zero();
	FileContext::ID file_id = FileContext::BUILT_IN_ID;

	TypeInfo::ID type_id       = register_type(std::move(type), span, file_id);
	TypeInfo     function_type = TypeInfo::make_function(
                TypeInfo::Function {
			    {{std::nullopt, type_id}, {std::nullopt, type_id}},
                        type_id
        }
        );
	identify_built_in_operator(function, operator_, std::move(function_type));
}

void Resolver::identify_built_in_binary_comparison_operator(
	IR::BuiltInFunction function,
	Token::Symbol       operator_,
	TypeInfo&&          type
) {
	Span            span    = Span::zero();
	FileContext::ID file_id = FileContext::BUILT_IN_ID;

	TypeInfo::ID type_id       = register_type(std::move(type), span, file_id);
	TypeInfo::ID return_id     = register_type(TypeInfo::make_known_bool(), span, file_id);
	TypeInfo     function_type = TypeInfo::make_function(
                TypeInfo::Function {
			    {{std::nullopt, type_id}, {std::nullopt, type_id}},
                        return_id
        }
        );
	identify_built_in_operator(function, operator_, std::move(function_type));
}

void Resolver::identify_built_in_operators() {
	// TODO: implement the integer operators for all kinds of integers through generics, once those are a thing. for
	// now, we will only implement them for 8, 16, 32, 64.

	// identify plus, minus, star, div for integers
	std::vector<uint32_t> integer_sizes {8, 16, 32, 64};
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
		for (uint32_t size : integer_sizes) {
			identify_built_in_binary_operator(
				unsigned_,
				operator_,
				TypeInfo::make_known_integer(
					TypeInfo::KnownInteger {
						AST::Type::Atom::Integer::with_width(size, false).value()
					}
				)
			);
			identify_built_in_binary_operator(
				signed_,
				operator_,
				TypeInfo::make_known_integer(
					TypeInfo::KnownInteger {
						AST::Type::Atom::Integer::with_width(size, true).value()
					}
				)
			);
		}
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
		for (uint32_t size : integer_sizes) {
			identify_built_in_binary_comparison_operator(
				unsigned_,
				operator_,
				TypeInfo::make_known_integer(
					TypeInfo::KnownInteger {
						AST::Type::Atom::Integer::with_width(size, false).value()
					}
				)
			);
			identify_built_in_binary_comparison_operator(
				signed_,
				operator_,
				TypeInfo::make_known_integer(
					TypeInfo::KnownInteger {
						AST::Type::Atom::Integer::with_width(size, true).value()
					}
				)
			);
		}
	}
	// identify unary negation for integers
	for (uint32_t size : integer_sizes)
		identify_built_in_unary_operator(
			IR::BuiltInFunction::NegateSInteger,
			Token::Symbol::Minus,
			TypeInfo::make_known_integer(
				TypeInfo::KnownInteger {AST::Type::Atom::Integer::with_width(size, true).value()}
			)
		);

	// identify plus, minus, star, div for floats
	std::vector<std::tuple<Token::Symbol, IR::BuiltInFunction>> binary_float_operators {
		{ Token::Symbol::Plus,      IR::BuiltInFunction::AddFloats},
		{Token::Symbol::Minus, IR::BuiltInFunction::SubtractFloats},
		{ Token::Symbol::Star, IR::BuiltInFunction::MultiplyFloats},
		{  Token::Symbol::Div,   IR::BuiltInFunction::DivideFloats}
	};
	std::vector<AST::Type::Atom::Float::Width> widths {
		AST::Type::Atom::Float::Width::F16,
		AST::Type::Atom::Float::Width::F32,
		AST::Type::Atom::Float::Width::F64,
		AST::Type::Atom::Float::Width::F128
	};
	for (auto [operator_, function] : binary_float_operators)
		for (auto width : widths)
			identify_built_in_binary_operator(
				function,
				operator_,
				TypeInfo::make_known_float(TypeInfo::KnownFloat {width})
			);
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
		for (auto width : widths)
			identify_built_in_binary_comparison_operator(
				function,
				operator_,
				TypeInfo::make_known_float(TypeInfo::KnownFloat {width})
			);
	// identify unary negation for floats
	for (auto width : widths)
		identify_built_in_unary_operator(
			IR::BuiltInFunction::NegateFloat,
			Token::Symbol::Minus,
			TypeInfo::make_known_float(TypeInfo::KnownFloat {width})
		);
	// identify equality and inequality operator for char and bool
	identify_built_in_binary_comparison_operator(
		IR::BuiltInFunction::EqChars,
		Token::Symbol::EqEq,
		TypeInfo::make_known_char()
	);
	identify_built_in_binary_comparison_operator(
		IR::BuiltInFunction::EqBools,
		Token::Symbol::EqEq,
		TypeInfo::make_known_bool()
	);
	identify_built_in_binary_comparison_operator(
		IR::BuiltInFunction::NeChars,
		Token::Symbol::Ne,
		TypeInfo::make_known_char()
	);
	identify_built_in_binary_comparison_operator(
		IR::BuiltInFunction::NeBools,
		Token::Symbol::Ne,
		TypeInfo::make_known_bool()
	);
	// identify unary boolean negation
	identify_built_in_unary_operator(
		IR::BuiltInFunction::NegateBool,
		Token::Symbol::Bang,
		TypeInfo::make_known_bool()
	);
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
