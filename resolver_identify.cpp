#include "ir.hpp"
#include "resolver.hpp"

#include <variant>

void Resolver::identify(AST::Identifier& identifier) {
	assert(!identifier.id.has_value());
	identifier.id = {symbol_next()};
}

void Resolver::identify(AST::Module& module, bool exported, FileContext::ID file_id) {
	// TODO: ensure there are no duplicated item names (including aliases)
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
	}
}

void Resolver::identify(AST::Function& function, bool exported, FileContext::ID file_id) {
	std::vector<std::tuple<std::optional<std::string>, TypeInfo::ID>> arguments {};
	arguments.reserve(function.arguments.size());

	for (auto& argument : function.arguments) {
		identify(argument.name.value);
		TypeInfo::ID type_id = register_type(
			TypeInfo::from_type(argument.type.value),
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
		= register_type(TypeInfo::from_type(function.return_type.value), function.return_type.span, file_id);

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
	// TODO: have designated sentinel values for these
	Span            span    = Span(0);
	FileContext::ID file_id = 42;

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
	// TODO: have designated sentinel values for these
	Span            span    = Span(0);
	FileContext::ID file_id = 42;

	TypeInfo::ID type_id       = register_type(std::move(type), span, file_id);
	TypeInfo     function_type = TypeInfo::make_function(TypeInfo::Function {{{std::nullopt, type_id}}, type_id});
	identify_built_in_operator(function, operator_, std::move(function_type));
}

void Resolver::identify_built_in_binary_operator(
	IR::BuiltInFunction function,
	Token::Symbol       operator_,
	TypeInfo&&          type
) {
	// TODO: have designated sentinel values for these
	Span            span    = Span(0);
	FileContext::ID file_id = 42;

	TypeInfo::ID type_id       = register_type(std::move(type), span, file_id);
	TypeInfo     function_type = TypeInfo::make_function(
                TypeInfo::Function {
			    {{std::nullopt, type_id}, {std::nullopt, type_id}},
                        type_id
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
	// identify unary negation for floats
	for (auto width : widths)
		identify_built_in_unary_operator(
			IR::BuiltInFunction::NegateFloat,
			Token::Symbol::Minus,
			TypeInfo::make_known_float(TypeInfo::KnownFloat {width})
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
	// we will deal with goto later!
	case AST::Statement::Kind::Goto:  return;
	case AST::Statement::Kind::Label: break;
	}

	if (statement.value.kind() == AST::Statement::Kind::Label) {
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
	case AST::Statement::Kind::Label: return;
	case AST::Statement::Kind::Goto:  break;
	}

	if (statement.value.kind() == AST::Statement::Kind::Goto) {
		std::string const& destination = statement.value.get_goto().destination;
		if (!labels.contains(destination)) {
			parsed_files.at(file_id).diagnostics.push_back(
				Diagnostic::error(
					"unknown label",
					{
						Diagnostic::Sample(
							get_context(file_id),
							statement.span,
							OutFmt::Color::Red
						),
					}
				)
			);
			return;
		}
		AST::Statement::Label::ID id = labels.at(destination).value;

		statement.value.get_goto().destination_id = id;
		return;
	}
}

void Resolver::identify_labels(AST::Function& function, FileContext::ID file_id) {
	if (!function.body.has_value()) return;
	std::unordered_map<std::string, Spanned<AST::Statement::Label::ID>> labels {};

	AST::Statement::Label::ID counter = 1;  // we save 0 for the entry
	for (Spanned<AST::Statement>& statement : function.body.value()) {
		identify_populate_labels(statement, labels, counter, file_id);
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
