#include "resolver.hpp"

#include <variant>

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
	                register_type(
				TypeInfo::make_module(),
				module.name.span,
				file_id,
				module.name.value.id.value()[0]
			),
	                false}
	);
	for (Spanned<AST::Module::Item>& item : module.body.items) {
		auto& value = std::get<AST::Module::InnerItem>(item.value);
		if (std::holds_alternative<AST::Function>(value)) identify(std::get<AST::Function>(value), file_id);
		else if (std::holds_alternative<AST::Module>(value)) identify(std::get<AST::Module>(value), file_id);
	}
}

void Resolver::identify(AST::Function& function, FileContext::ID file_id) {
	std::vector<std::tuple<std::optional<std::string>, TypeInfo::ID>> arguments {};
	arguments.reserve(function.arguments.size());

	for (auto& argument : function.arguments) {
		// TODO: mutable arguments
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
		                false}
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
	                false}
	);
}

void Resolver::identify_module_items() {
	for (ParsedFile& file : parsed_files) { identify(file.module, file.file_id); }
}
