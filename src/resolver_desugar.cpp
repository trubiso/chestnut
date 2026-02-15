#include "ast/statement.hpp"
#include "resolver.hpp"

#include <variant>

std::vector<Spanned<AST::Statement>> Resolver::desugar_control_flow_expr(
	AST::Expression::Atom&     atom,
	Span                       span,
	AST::Statement::Label::ID& label_counter,
	FileContext::ID            file_id
) {
	switch (atom.kind()) {
	case AST::Expression::Atom::Kind::Identifier:
	case AST::Expression::Atom::Kind::NumberLiteral:
	case AST::Expression::Atom::Kind::StringLiteral:
	case AST::Expression::Atom::Kind::CharLiteral:
	case AST::Expression::Atom::Kind::BoolLiteral:   return {};
	case AST::Expression::Atom::Kind::StructLiteral: break;
	case AST::Expression::Atom::Kind::Expression:
		return desugar_control_flow_expr(*atom.get_expression(), span, label_counter, file_id);
	}

	// for struct literals, we want to desugar each field
	std::vector<Spanned<AST::Statement>> stmts {};
	for (auto& field : atom.get_struct_literal().fields) {
		std::vector<Spanned<AST::Statement>> extra_stmts
			= desugar_control_flow_expr(*field.value, label_counter, file_id);
		std::move(extra_stmts.begin(), extra_stmts.end(), std::back_inserter(stmts));
	}
	return stmts;
}

std::vector<Spanned<AST::Statement>> Resolver::desugar_control_flow_expr(
	AST::Expression::UnaryOperation& unary_operation,
	AST::Statement::Label::ID&       label_counter,
	FileContext::ID                  file_id
) {
	return desugar_control_flow_expr(*unary_operation.operand, label_counter, file_id);
}

std::vector<Spanned<AST::Statement>> Resolver::desugar_control_flow_expr(
	AST::Expression::AddressOperation& address_operation,
	AST::Statement::Label::ID&         label_counter,
	FileContext::ID                    file_id
) {
	return desugar_control_flow_expr(*address_operation.operand, label_counter, file_id);
}

std::vector<Spanned<AST::Statement>> Resolver::desugar_control_flow_expr(
	AST::Expression::BinaryOperation& binary_operation,
	AST::Statement::Label::ID&        label_counter,
	FileContext::ID                   file_id
) {
	std::vector<Spanned<AST::Statement>> stmts
		= desugar_control_flow_expr(*binary_operation.lhs, label_counter, file_id);
	std::vector<Spanned<AST::Statement>> extra_stmts
		= desugar_control_flow_expr(*binary_operation.rhs, label_counter, file_id);
	std::move(extra_stmts.begin(), extra_stmts.end(), std::back_inserter(stmts));
	return stmts;
}

std::vector<Spanned<AST::Statement>> Resolver::desugar_control_flow_expr(
	AST::Expression::FunctionCall& function_call,
	Span                           span,
	AST::Statement::Label::ID&     label_counter,
	FileContext::ID                file_id
) {
	std::vector<Spanned<AST::Statement>> stmts
		= desugar_control_flow_expr(*function_call.callee, label_counter, file_id);
	for (auto& argument : function_call.arguments.ordered) {
		std::vector<Spanned<AST::Statement>> extra_stmts
			= desugar_control_flow_expr(argument, label_counter, file_id);
		std::move(extra_stmts.begin(), extra_stmts.end(), std::back_inserter(stmts));
	}
	for (auto& [name, argument] : function_call.arguments.labeled) {
		std::vector<Spanned<AST::Statement>> extra_stmts
			= desugar_control_flow_expr(argument, label_counter, file_id);
		std::move(extra_stmts.begin(), extra_stmts.end(), std::back_inserter(stmts));
	}
	return stmts;
}

std::vector<Spanned<AST::Statement>> Resolver::desugar_control_flow_expr(
	AST::Expression::MemberAccess& member_access,
	Span                           span,
	AST::Statement::Label::ID&     label_counter,
	FileContext::ID                file_id
) {
	return desugar_control_flow_expr(*member_access.accessee, label_counter, file_id);
}

std::vector<Spanned<AST::Statement>> Resolver::desugar_control_flow_expr_binop(
	AST::Expression&           expression,
	Span                       span,
	AST::Statement::Label::ID& label_counter,
	FileContext::ID            file_id
) {
	// we only want a special case for logical operators
	switch (expression.get_binary_operation().operation) {
	case Token::Symbol::AmpAmp:
	case Token::Symbol::BarBar: break;
	default:                    return desugar_control_flow_expr(expression.get_binary_operation(), label_counter, file_id);
	}

	std::vector<Spanned<AST::Statement>> stmts {};

	auto binary_operation = std::move(expression.get_binary_operation());
	bool is_or            = binary_operation.operation == Token::Symbol::BarBar;

	std::vector<Spanned<AST::Statement>> lhs_stmts
		= desugar_control_flow_expr(*binary_operation.lhs, label_counter, file_id);
	std::vector<Spanned<AST::Statement>> rhs_stmts
		= desugar_control_flow_expr(*binary_operation.rhs, label_counter, file_id);

	// for logical operators, we want to create a variable, default it to true/false and then add a branch. that
	// way, we only compute valuables whenever they are necessary.
	// FIXME: these spans are not correct
	Span stub_span = span;

	AST::SymbolID new_id  = symbol_next();
	TypeInfo::ID  type_id = register_type(TypeInfo::make_known_bool(), stub_span, file_id, new_id);
	symbol_pool_.push_back(Symbol {new_id, file_id, stub_span, "_", std::monostate {}, type_id, true, false, {}});
	AST::Identifier new_var {
		{stub_span, "_"}
	};
	new_var.id = {new_id};

	AST::Expression value = AST::Expression::make_atom(AST::Expression::Atom::make_bool_literal(is_or));

	stmts.emplace_back(
		stub_span,
		AST::Statement::make_declare(
			AST::Statement::Declare {
				{stub_span, new_var},
				{{stub_span, AST::Type::make_atom(AST::Type::Atom::make_bool())}},
				{{stub_span, std::move(value)}},
				{stub_span, true},
				false
        }
		)
	);

	AST::Identifier new_var1 = new_var;

	AST::Expression new_var_expr
		= AST::Expression::make_atom(AST::Expression::Atom::make_identifier(std::move(new_var)));
	AST::Expression new_var_expr1
		= AST::Expression::make_atom(AST::Expression::Atom::make_identifier(std::move(new_var1)));

	AST::Statement::Label set_label {"set", label_counter++};
	AST::Statement::Label cont_label {"cont", label_counter++};
	AST::Statement::Goto  goto_set {set_label.name, set_label.id};
	AST::Statement::Goto  goto_cont {cont_label.name, cont_label.id};

	AST::Statement::Branch branch {
		std::move(std::move(*binary_operation.lhs)),
		{stub_span, is_or ? goto_cont : goto_set},
		{{stub_span, is_or ? goto_set : goto_cont}}
	};

	AST::Statement::Set set {
		{stub_span, std::move(new_var_expr1)},
		std::move(*binary_operation.rhs)
	};

	std::move(lhs_stmts.begin(), lhs_stmts.end(), std::back_inserter(stmts));
	stmts.emplace_back(stub_span, AST::Statement::make_branch(std::move(branch)));
	stmts.emplace_back(stub_span, AST::Statement::make_label(std::move(set_label)));
	std::move(rhs_stmts.begin(), rhs_stmts.end(), std::back_inserter(stmts));
	stmts.emplace_back(stub_span, AST::Statement::make_set(std::move(set)));
	stmts.emplace_back(stub_span, AST::Statement::make_label(std::move(cont_label)));

	// and finally we can replace the expression
	expression = std::move(new_var_expr);

	return stmts;
}

std::vector<Spanned<AST::Statement>> Resolver::desugar_control_flow_expr_if(
	AST::Expression&           expression,
	Span                       span,
	AST::Statement::Label::ID& label_counter,
	FileContext::ID            file_id
) {
	AST::Expression::If                  if_   = std::move(expression.get_if());
	std::vector<Spanned<AST::Statement>> stmts = desugar_control_flow_expr(*if_.condition, label_counter, file_id);
	std::vector<Spanned<AST::Statement>> true_stmts = desugar_control_flow_expr(*if_.true_, label_counter, file_id);
	std::vector<Spanned<AST::Statement>> false_stmts
		= desugar_control_flow_expr(*if_.false_, label_counter, file_id);

	// for if expressions, we want to declare a new anonymous variable, branch on the condition to set its value and
	// then replace this expression with an identifier pointing to that variable.
	// FIXME: these spans are not correct
	Span stub_span = span;

	AST::SymbolID new_id  = symbol_next();
	TypeInfo::ID  type_id = register_type(TypeInfo::make_unknown(), stub_span, file_id, new_id);
	symbol_pool_.push_back(Symbol {new_id, file_id, stub_span, "_", std::monostate {}, type_id, false, false, {}});
	AST::Identifier new_var {
		{stub_span, "_"}
	};
	new_var.id = {new_id};

	stmts.emplace_back(
		stub_span,
		AST::Statement::make_declare(
			AST::Statement::Declare {
				{stub_span, new_var},
				std::nullopt,
				std::nullopt,
				{stub_span,   false},
				true
        }
		)
	);

	AST::Statement::Label true_label {"true", label_counter++};
	AST::Statement::Label false_label {"false", label_counter++};
	AST::Statement::Label cont_label {"cont", label_counter++};

	AST::Statement::Goto goto_true {true_label.name, true_label.id};
	AST::Statement::Goto goto_false {false_label.name, false_label.id};
	AST::Statement::Goto goto_cont {cont_label.name, cont_label.id};
	AST::Statement::Goto goto_cont1 = goto_cont;

	AST::Statement::Branch branch {
		std::move(*if_.condition),
		{stub_span, goto_true},
		{{stub_span, goto_false}}
	};

	AST::Identifier new_var1 = new_var, new_var2 = new_var;

	AST::Expression new_var_expr
		= AST::Expression::make_atom(AST::Expression::Atom::make_identifier(std::move(new_var)));
	AST::Expression new_var_expr1
		= AST::Expression::make_atom(AST::Expression::Atom::make_identifier(std::move(new_var1)));
	AST::Expression new_var_expr2
		= AST::Expression::make_atom(AST::Expression::Atom::make_identifier(std::move(new_var2)));
	new_var_expr.type = new_var_expr1.type = new_var_expr2.type = type_id;

	AST::Statement::Set set_true {
		{stub_span, std::move(new_var_expr1)},
		std::move(*if_.true_)
	};
	AST::Statement::Set set_false {
		{stub_span, std::move(new_var_expr2)},
		std::move(*if_.false_)
	};

	// now we can just add the statements we want in order
	stmts.emplace_back(stub_span, AST::Statement::make_branch(std::move(branch)));
	stmts.emplace_back(stub_span, AST::Statement::make_label(std::move(true_label)));
	std::move(true_stmts.begin(), true_stmts.end(), std::back_inserter(stmts));
	stmts.emplace_back(stub_span, AST::Statement::make_set(std::move(set_true)));
	stmts.emplace_back(stub_span, AST::Statement::make_goto(std::move(goto_cont)));
	stmts.emplace_back(stub_span, AST::Statement::make_label(std::move(false_label)));
	std::move(false_stmts.begin(), false_stmts.end(), std::back_inserter(stmts));
	stmts.emplace_back(stub_span, AST::Statement::make_set(std::move(set_false)));
	stmts.emplace_back(stub_span, AST::Statement::make_goto(std::move(goto_cont1)));
	stmts.emplace_back(stub_span, AST::Statement::make_label(std::move(cont_label)));

	// and finally we can replace the expression
	expression = std::move(new_var_expr);

	return stmts;
}

std::vector<Spanned<AST::Statement>> Resolver::desugar_control_flow_expr(
	AST::Expression&           expression,
	Span                       span,
	AST::Statement::Label::ID& label_counter,
	FileContext::ID            file_id
) {
	switch (expression.kind()) {
	case AST::Expression::Kind::Atom:
		return desugar_control_flow_expr(expression.get_atom(), span, label_counter, file_id);
	case AST::Expression::Kind::UnaryOperation:
		return desugar_control_flow_expr(expression.get_unary_operation(), label_counter, file_id);
	case AST::Expression::Kind::AddressOperation:
		return desugar_control_flow_expr(expression.get_address_operation(), label_counter, file_id);
	case AST::Expression::Kind::BinaryOperation:
		return desugar_control_flow_expr_binop(expression, span, label_counter, file_id);
	case AST::Expression::Kind::FunctionCall:
		return desugar_control_flow_expr(expression.get_function_call(), span, label_counter, file_id);
	case AST::Expression::Kind::If: return desugar_control_flow_expr_if(expression, span, label_counter, file_id);
	case AST::Expression::Kind::MemberAccess:
		return desugar_control_flow_expr(expression.get_member_access(), span, label_counter, file_id);
	}
}

std::vector<Spanned<AST::Statement>> Resolver::desugar_control_flow_expr(
	Spanned<AST::Expression>&  expression,
	AST::Statement::Label::ID& label_counter,
	FileContext::ID            file_id
) {
	return desugar_control_flow_expr(expression.value, expression.span, label_counter, file_id);
}

std::vector<Spanned<AST::Statement>> Resolver::desugar_control_flow(
	AST::Statement::Declare&&  declare,
	Span                       span,
	AST::Statement::Label::ID& label_counter,
	FileContext::ID            file_id
) {
	std::vector<Spanned<AST::Statement>> stmts
		= declare.value.has_value() ? desugar_control_flow_expr(declare.value.value(), label_counter, file_id)
	                                    : std::vector<Spanned<AST::Statement>> {};
	stmts.emplace_back(span, AST::Statement::make_declare(std::move(declare)));
	return stmts;
}

std::vector<Spanned<AST::Statement>> Resolver::desugar_control_flow(
	AST::Statement::Set&&      set,
	Span                       span,
	AST::Statement::Label::ID& label_counter,
	FileContext::ID            file_id
) {
	std::vector<Spanned<AST::Statement>> stmts       = desugar_control_flow_expr(set.rhs, label_counter, file_id);
	std::vector<Spanned<AST::Statement>> extra_stmts = desugar_control_flow_expr(set.lhs, label_counter, file_id);
	std::move(extra_stmts.begin(), extra_stmts.end(), std::back_inserter(stmts));
	stmts.emplace_back(span, AST::Statement::make_set(std::move(set)));
	return stmts;
}

std::vector<Spanned<AST::Statement>> Resolver::desugar_control_flow(
	AST::Expression&&          expression,
	Span                       span,
	AST::Statement::Label::ID& label_counter,
	FileContext::ID            file_id
) {
	Spanned<AST::Expression>             new_expression {span, std::move(expression)};
	std::vector<Spanned<AST::Statement>> stmts = desugar_control_flow_expr(new_expression, label_counter, file_id);
	stmts.emplace_back(new_expression.span, AST::Statement::make_expression(std::move(new_expression.value)));
	return stmts;
}

std::vector<Spanned<AST::Statement>> Resolver::desugar_control_flow(
	AST::Statement::Return&&   return_,
	Span                       span,
	AST::Statement::Label::ID& label_counter,
	FileContext::ID            file_id
) {
	std::vector<Spanned<AST::Statement>> stmts
		= return_.value.has_value() ? desugar_control_flow_expr(return_.value.value(), label_counter, file_id)
	                                    : std::vector<Spanned<AST::Statement>> {};
	stmts.emplace_back(span, AST::Statement::make_return(std::move(return_)));
	return stmts;
}

std::vector<Spanned<AST::Statement>> Resolver::desugar_control_flow(
	AST::Statement::Branch&&   branch,
	Span                       span,
	AST::Statement::Label::ID& label_counter,
	FileContext::ID            file_id
) {
	std::vector<Spanned<AST::Statement>> stmts
		= desugar_control_flow_expr(branch.condition, label_counter, file_id);
	stmts.emplace_back(span, AST::Statement::make_branch(std::move(branch)));
	return stmts;
}

std::vector<Spanned<AST::Statement>> Resolver::desugar_control_flow(
	AST::Statement::If&&       if_,
	Span                       span,
	AST::Statement::Label::ID& label_counter,
	FileContext::ID            file_id
) {
	Spanned<AST::Expression>             condition = std::move(if_.condition);
	std::vector<Spanned<AST::Statement>> stmts     = desugar_control_flow_expr(condition, label_counter, file_id);
	// FIXME: use the correct spans ("if" for the true case and mby the branch stmt, "else" for the
	// false case)
	Span stub_span = span;

	Spanned<AST::Statement::Goto> goto_true {
		stub_span,
		{"true", label_counter++}
	};

	auto goto_false = if_.false_.has_value() ? std::optional<Spanned<AST::Statement::Goto>> {{stub_span, {"false", label_counter++}}} : std::nullopt;

	AST::Statement::Goto goto_cont {"cont", label_counter++};

	AST::Statement::Branch branch {
		std::move(condition),
		goto_true,
		goto_false.has_value() ? goto_false.value() : Spanned {stub_span, goto_cont}
	};

	// now we need to create the branch, then true, then false, then cont label

	// branch
	stmts.emplace_back(stub_span, AST::Statement::make_branch(std::move(branch)));

	// true
	stmts.emplace_back(
		stub_span,
		AST::Statement::make_label(AST::Statement::Label {"true", goto_true.value.destination_id})
	);
	stmts.emplace_back(
		if_.true_.span,
		AST::Statement::make_scope(desugar_control_flow(std::move(if_.true_.value), label_counter, file_id))
	);
	auto goto_1 = goto_cont;
	stmts.emplace_back(stub_span, AST::Statement::make_goto(std::move(goto_1), true));

	// false
	if (goto_false.has_value()) {
		stmts.emplace_back(
			stub_span,
			AST::Statement::make_label(
				AST::Statement::Label {"false", goto_false.value().value.destination_id}
			)
		);
		stmts.emplace_back(
			if_.false_.value().span,
			AST::Statement::make_scope(
				desugar_control_flow(std::move(if_.false_.value().value), label_counter, file_id)
			)
		);
		// TODO: don't add these post-gotos if the scope ends on a branch/goto
		auto goto_2 = goto_cont;
		stmts.emplace_back(stub_span, AST::Statement::make_goto(std::move(goto_2), true));
	}

	// cont
	stmts.emplace_back(
		stub_span,
		AST::Statement::make_label(AST::Statement::Label {"cont", goto_cont.destination_id})
	);

	return stmts;
}

std::vector<Spanned<AST::Statement>> Resolver::desugar_control_flow(
	AST::Statement&&           statement,
	Span                       span,
	AST::Statement::Label::ID& label_counter,
	FileContext::ID            file_id
) {
	switch (statement.kind()) {
	case AST::Statement::Kind::Declare:
		return desugar_control_flow(std::move(statement.get_declare()), span, label_counter, file_id);
	case AST::Statement::Kind::Set:
		return desugar_control_flow(std::move(statement.get_set()), span, label_counter, file_id);
	case AST::Statement::Kind::Expression:
		return desugar_control_flow(std::move(statement.get_expression()), span, label_counter, file_id);
	case AST::Statement::Kind::Return:
		return desugar_control_flow(std::move(statement.get_return()), span, label_counter, file_id);
	case AST::Statement::Kind::Scope:
		statement.get_scope() = desugar_control_flow(std::move(statement.get_scope()), label_counter, file_id);
		break;
	case AST::Statement::Kind::Label:
	case AST::Statement::Kind::Goto:  break;
	case AST::Statement::Kind::Branch:
		return desugar_control_flow(std::move(statement.get_branch()), span, label_counter, file_id);
	case AST::Statement::Kind::If:
		return desugar_control_flow(std::move(statement.get_if()), span, label_counter, file_id);
	}

	// scope, label and goto are not evil, so we can package them into a little vector :D
	std::vector<Spanned<AST::Statement>> output {};
	output.emplace_back(span, std::move(statement));
	return output;
}

AST::Scope
Resolver::desugar_control_flow(AST::Scope&& scope, AST::Statement::Label::ID& label_counter, FileContext::ID file_id) {
	std::vector<Spanned<AST::Statement>> new_scope {};
	new_scope.reserve(scope.size());
	for (Spanned<AST::Statement>& statement : scope) {
		std::vector<Spanned<AST::Statement>> new_stmts
			= desugar_control_flow(std::move(statement.value), statement.span, label_counter, file_id);
		std::move(new_stmts.begin(), new_stmts.end(), std::back_inserter(new_scope));
	}
	return new_scope;
}

void Resolver::desugar_control_flow(AST::Function& function, FileContext::ID file_id) {
	if (!function.body.has_value()) return;
	// we initialize the label counter for everyone else. we reserve 0 for the entry block
	function.label_counter = 1;
	function.body = desugar_control_flow(std::move(function.body.value()), function.label_counter, file_id);
}

void Resolver::desugar_control_flow(AST::Module& module, FileContext::ID file_id) {
	for (Spanned<AST::Module::Item>& item : module.body.items) {
		auto& value = std::get<AST::Module::InnerItem>(item.value);
		if (std::holds_alternative<AST::Function>(value))
			desugar_control_flow(std::get<AST::Function>(value), file_id);
		else if (std::holds_alternative<AST::Module>(value))
			desugar_control_flow(std::get<AST::Module>(value), file_id);
	}
}

void Resolver::desugar_control_flow() {
	for (ParsedFile& file : parsed_files) desugar_control_flow(file.module, file.file_id);
}
