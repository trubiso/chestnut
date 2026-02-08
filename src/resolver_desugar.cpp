#include "resolver.hpp"

#include <variant>

std::vector<Spanned<AST::Statement>> Resolver::desugar_control_flow_expr(
	AST::Expression::UnaryOperation& unary_operation,
	AST::Statement::Label::ID&       label_counter
) {
	return desugar_control_flow_expr(*unary_operation.operand, label_counter);
}

std::vector<Spanned<AST::Statement>> Resolver::desugar_control_flow_expr(
	AST::Expression::BinaryOperation& binary_operation,
	AST::Statement::Label::ID&        label_counter
) {
	std::vector<Spanned<AST::Statement>> stmts = desugar_control_flow_expr(*binary_operation.lhs, label_counter);
	std::vector<Spanned<AST::Statement>> extra_stmts
		= desugar_control_flow_expr(*binary_operation.rhs, label_counter);
	std::move(extra_stmts.begin(), extra_stmts.end(), std::back_inserter(stmts));
	return stmts;
}

std::vector<Spanned<AST::Statement>> Resolver::desugar_control_flow_expr(
	AST::Expression::FunctionCall& function_call,
	Span                           span,
	AST::Statement::Label::ID&     label_counter
) {
	std::vector<Spanned<AST::Statement>> stmts = desugar_control_flow_expr(*function_call.callee, label_counter);
	for (auto& argument : function_call.arguments.ordered) {
		std::vector<Spanned<AST::Statement>> extra_stmts = desugar_control_flow_expr(argument, label_counter);
		std::move(extra_stmts.begin(), extra_stmts.end(), std::back_inserter(stmts));
	}
	for (auto& [name, argument] : function_call.arguments.labeled) {
		std::vector<Spanned<AST::Statement>> extra_stmts = desugar_control_flow_expr(argument, label_counter);
		std::move(extra_stmts.begin(), extra_stmts.end(), std::back_inserter(stmts));
	}
	return stmts;
}

std::vector<Spanned<AST::Statement>>
Resolver::desugar_control_flow_expr(AST::Expression::If& if_, Span span, AST::Statement::Label::ID& label_counter) {
	std::vector<Spanned<AST::Statement>> stmts        = desugar_control_flow_expr(*if_.condition, label_counter);
	std::vector<Spanned<AST::Statement>> extra_stmts1 = desugar_control_flow_expr(*if_.true_, label_counter);
	std::move(extra_stmts1.begin(), extra_stmts1.end(), std::back_inserter(stmts));
	std::vector<Spanned<AST::Statement>> extra_stmts2 = desugar_control_flow_expr(*if_.false_, label_counter);
	std::move(extra_stmts2.begin(), extra_stmts2.end(), std::back_inserter(stmts));

	// TODO: desugar if expressions

	return stmts;
}

std::vector<Spanned<AST::Statement>>
Resolver::desugar_control_flow_expr(AST::Expression& expression, Span span, AST::Statement::Label::ID& label_counter) {
	switch (expression.kind()) {
	case AST::Expression::Kind::Atom: return {};
	case AST::Expression::Kind::UnaryOperation:
		return desugar_control_flow_expr(expression.get_unary_operation(), label_counter);
	case AST::Expression::Kind::BinaryOperation:
		return desugar_control_flow_expr(expression.get_binary_operation(), label_counter);
	case AST::Expression::Kind::FunctionCall:
		return desugar_control_flow_expr(expression.get_function_call(), span, label_counter);
	case AST::Expression::Kind::If: return desugar_control_flow_expr(expression.get_if(), span, label_counter);
	}
}

std::vector<Spanned<AST::Statement>>
Resolver::desugar_control_flow_expr(Spanned<AST::Expression>& expression, AST::Statement::Label::ID& label_counter) {
	return desugar_control_flow_expr(expression.value, expression.span, label_counter);
}

std::vector<Spanned<AST::Statement>>
Resolver::desugar_control_flow(AST::Statement::Declare&& declare, Span span, AST::Statement::Label::ID& label_counter) {
	std::vector<Spanned<AST::Statement>> stmts
		= declare.value.has_value() ? desugar_control_flow_expr(declare.value.value(), label_counter)
	                                    : std::vector<Spanned<AST::Statement>> {};
	stmts.emplace_back(span, AST::Statement::make_declare(std::move(declare)));
	return stmts;
}

std::vector<Spanned<AST::Statement>>
Resolver::desugar_control_flow(AST::Statement::Set&& set, Span span, AST::Statement::Label::ID& label_counter) {
	std::vector<Spanned<AST::Statement>> stmts       = desugar_control_flow_expr(set.rhs, label_counter);
	std::vector<Spanned<AST::Statement>> extra_stmts = desugar_control_flow_expr(set.lhs, label_counter);
	std::move(extra_stmts.begin(), extra_stmts.end(), std::back_inserter(stmts));
	stmts.emplace_back(span, AST::Statement::make_set(std::move(set)));
	return stmts;
}

std::vector<Spanned<AST::Statement>>
Resolver::desugar_control_flow(AST::Expression&& expression, Span span, AST::Statement::Label::ID& label_counter) {
	Spanned<AST::Expression>             new_expression {span, std::move(expression)};
	std::vector<Spanned<AST::Statement>> stmts = desugar_control_flow_expr(new_expression, label_counter);
	stmts.emplace_back(new_expression.span, AST::Statement::make_expression(std::move(new_expression.value)));
	return stmts;
}

std::vector<Spanned<AST::Statement>>
Resolver::desugar_control_flow(AST::Statement::Return&& return_, Span span, AST::Statement::Label::ID& label_counter) {
	std::vector<Spanned<AST::Statement>> stmts
		= return_.value.has_value() ? desugar_control_flow_expr(return_.value.value(), label_counter)
	                                    : std::vector<Spanned<AST::Statement>> {};
	stmts.emplace_back(span, AST::Statement::make_return(std::move(return_)));
	return stmts;
}

std::vector<Spanned<AST::Statement>>
Resolver::desugar_control_flow(AST::Statement::Branch&& branch, Span span, AST::Statement::Label::ID& label_counter) {
	std::vector<Spanned<AST::Statement>> stmts = desugar_control_flow_expr(branch.condition, label_counter);
	stmts.emplace_back(span, AST::Statement::make_branch(std::move(branch)));
	return stmts;
}

std::vector<Spanned<AST::Statement>>
Resolver::desugar_control_flow(AST::Statement::If&& if_, Span span, AST::Statement::Label::ID& label_counter) {
	std::vector<Spanned<AST::Statement>> stmts {};
	// FIXME: use the correct spans ("if" for the true case and mby the branch stmt, "else" for the
	// false case)
	Span stub_span = span;

	Spanned<AST::Statement::Goto> goto_true {
		stub_span,
		{"true", label_counter++}
	};

	auto goto_false = if_.false_.has_value() ? std::optional<Spanned<AST::Statement::Goto>> {{stub_span, {"false", label_counter++}}} : std::nullopt;

	AST::Statement::Goto goto_cont {"cont", label_counter++};

	AST::Statement::Branch branch {std::move(if_.condition), goto_true, goto_false};

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
		AST::Statement::make_scope(desugar_control_flow(std::move(if_.true_.value), label_counter))
	);
	// TODO: don't add these post-gotos if the scope ends on a branch/goto
	auto goto_1 = goto_cont;
	stmts.emplace_back(stub_span, AST::Statement::make_goto(std::move(goto_1)));

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
				desugar_control_flow(std::move(if_.false_.value().value), label_counter)
			)
		);
		// TODO: don't add these post-gotos if the scope ends on a branch/goto
		auto goto_2 = goto_cont;
		stmts.emplace_back(stub_span, AST::Statement::make_goto(std::move(goto_2)));
	}

	// cont
	stmts.emplace_back(
		stub_span,
		AST::Statement::make_label(AST::Statement::Label {"cont", goto_cont.destination_id})
	);

	return stmts;
}

std::vector<Spanned<AST::Statement>>
Resolver::desugar_control_flow(AST::Statement&& statement, Span span, AST::Statement::Label::ID& label_counter) {
	switch (statement.kind()) {
	case AST::Statement::Kind::Declare:
		return desugar_control_flow(std::move(statement.get_declare()), span, label_counter);
	case AST::Statement::Kind::Set:
		return desugar_control_flow(std::move(statement.get_set()), span, label_counter);
	case AST::Statement::Kind::Expression:
		return desugar_control_flow(std::move(statement.get_expression()), span, label_counter);
	case AST::Statement::Kind::Return:
		return desugar_control_flow(std::move(statement.get_return()), span, label_counter);
	case AST::Statement::Kind::Scope:
		statement.get_scope() = desugar_control_flow(std::move(statement.get_scope()), label_counter);
		break;
	case AST::Statement::Kind::Label:
	case AST::Statement::Kind::Goto:  break;
	case AST::Statement::Kind::Branch:
		return desugar_control_flow(std::move(statement.get_branch()), span, label_counter);
	case AST::Statement::Kind::If: return desugar_control_flow(std::move(statement.get_if()), span, label_counter);
	}

	// scope, label and goto are not evil, so we can package them into a little vector :D
	std::vector<Spanned<AST::Statement>> output {};
	output.emplace_back(span, std::move(statement));
	return output;
}

AST::Scope Resolver::desugar_control_flow(AST::Scope&& scope, AST::Statement::Label::ID& label_counter) {
	std::vector<Spanned<AST::Statement>> new_scope {};
	new_scope.reserve(scope.size());
	for (Spanned<AST::Statement>& statement : scope) {
		std::vector<Spanned<AST::Statement>> new_stmts
			= desugar_control_flow(std::move(statement.value), statement.span, label_counter);
		std::move(new_stmts.begin(), new_stmts.end(), std::back_inserter(new_scope));
	}
	return new_scope;
}

void Resolver::desugar_control_flow(AST::Function& function) {
	if (!function.body.has_value()) return;
	function.body = desugar_control_flow(std::move(function.body.value()), function.label_counter);
}

void Resolver::desugar_control_flow(AST::Module& module) {
	for (Spanned<AST::Module::Item>& item : module.body.items) {
		auto& value = std::get<AST::Module::InnerItem>(item.value);
		if (std::holds_alternative<AST::Function>(value)) desugar_control_flow(std::get<AST::Function>(value));
		else if (std::holds_alternative<AST::Module>(value)) desugar_control_flow(std::get<AST::Module>(value));
	}
}

void Resolver::desugar_control_flow() {
	for (ParsedFile& file : parsed_files) desugar_control_flow(file.module);
}
