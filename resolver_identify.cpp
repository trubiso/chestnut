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
		auto [unsigned_, signed_] = functions;
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
