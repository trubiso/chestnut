#include "analyzer.hpp"

#include <sstream>
#include <variant>

void Analyzer::analyze() {
	check_assigned();
}

FileContext Analyzer::get_context(FileContext::ID file_id) const {
	if (file_id == FileContext::BUILT_IN_ID) return FileContext {"", file_id, {}, ""};
	return FileContext {
		resolved_files.at(file_id).name,
		file_id,
		resolved_files.at(file_id).loc,
		resolved_files.at(file_id).source,
	};
}

void Analyzer::check_assigned(
	IR::Identifier                                  identifier,
	Span                                            span,
	FileContext::ID                                 file_id,
	std::unordered_map<IR::Identifier, bool> const& assigned
) {
	if (!assigned.contains(identifier)) return;
	if (!assigned.at(identifier))
		resolved_files.at(file_id).diagnostics.push_back(
			Diagnostic::error(
				"used variable before it was assigned a value",
				{Diagnostic::Sample(get_context(file_id), span, OutFmt::Color::Red)}
			)
		);
}

void Analyzer::check_assigned(
	Spanned<IR::Identifier> const&                  identifier,
	FileContext::ID                                 file_id,
	std::unordered_map<IR::Identifier, bool> const& assigned
) {
	return check_assigned(identifier.value, identifier.span, file_id, assigned);
}

void Analyzer::check_assigned(
	IR::Expression::Atom const&                     atom,
	Span                                            span,
	FileContext::ID                                 file_id,
	std::unordered_map<IR::Identifier, bool> const& assigned
) {
	switch (atom.kind()) {
	case IR::Expression::Atom::Kind::Literal:
	case IR::Expression::Atom::Kind::Bool:
	case IR::Expression::Atom::Kind::Error:   return;
	case IR::Expression::Atom::Kind::Identifier:
		return check_assigned(atom.get_identifier(), span, file_id, assigned);
	case IR::Expression::Atom::Kind::StructLiteral: break;
	}

	// for struct literals, we must check all field values
	for (auto const& field : atom.get_struct_literal().fields) check_assigned(field, file_id, assigned);
}

void Analyzer::check_assigned(
	IR::Expression::FunctionCall const&             function_call,
	FileContext::ID                                 file_id,
	std::unordered_map<IR::Identifier, bool> const& assigned
) {
	if (std::holds_alternative<Spanned<IR::Identifier>>(function_call.callee))
		check_assigned(std::get<Spanned<IR::Identifier>>(function_call.callee), file_id, assigned);
	for (auto const& argument : function_call.arguments) check_assigned(argument, file_id, assigned);
}

void Analyzer::check_assigned(
	IR::Expression::Deref const&                    deref,
	FileContext::ID                                 file_id,
	std::unordered_map<IR::Identifier, bool> const& assigned
) {
	check_assigned(deref.address, file_id, assigned);
}

void Analyzer::check_assigned(
	IR::Expression::Ref const&                      ref,
	FileContext::ID                                 file_id,
	std::unordered_map<IR::Identifier, bool> const& assigned
) {
	check_assigned(ref.value, file_id, assigned);
}

void Analyzer::check_assigned(
	IR::Expression::MemberAccess const&             member_access,
	FileContext::ID                                 file_id,
	std::unordered_map<IR::Identifier, bool> const& assigned
) {
	check_assigned(member_access.accessee, file_id, assigned);
}

void Analyzer::check_assigned(
	Spanned<IR::Expression::Atom> const&            atom,
	FileContext::ID                                 file_id,
	std::unordered_map<IR::Identifier, bool> const& assigned
) {
	return check_assigned(atom.value, atom.span, file_id, assigned);
}

void Analyzer::check_assigned(
	Spanned<IR::Expression> const&                  expression,
	FileContext::ID                                 file_id,
	std::unordered_map<IR::Identifier, bool> const& assigned
) {
	switch (expression.value.kind()) {
	case IR::Expression::Kind::Atom:
		return check_assigned(expression.value.get_atom(), expression.span, file_id, assigned);
	case IR::Expression::Kind::FunctionCall:
		return check_assigned(expression.value.get_function_call(), file_id, assigned);
	case IR::Expression::Kind::Deref: return check_assigned(expression.value.get_deref(), file_id, assigned);
	case IR::Expression::Kind::Ref:   return check_assigned(expression.value.get_ref(), file_id, assigned);
	case IR::Expression::Kind::MemberAccess:
		return check_assigned(expression.value.get_member_access(), file_id, assigned);
	}
}

void Analyzer::check_assigned(
	IR::Statement::Declare&                   declare,
	FileContext::ID                           file_id,
	std::unordered_map<IR::Identifier, bool>& assigned
) {
	assigned.insert({declare.name.value, false});
}

void Analyzer::check_assigned(
	IR::Statement::Set&                       set,
	FileContext::ID                           file_id,
	std::unordered_map<IR::Identifier, bool>& assigned
) {
	check_assigned(set.value, file_id, assigned);
	if (assigned.contains(set.name.value)) {
		// TODO: it would be nice to find the first mutation span
		if (assigned.at(set.name.value) && !symbols.at(set.name.value).mutable_) {
			// mutability violation
			IR::Symbol const& symbol = symbols.at(set.name.value);
			std::stringstream subtitle {};
			subtitle
				<< "variable '"
				<< symbol.name
				<< "' was declared as constant.\ntip: you can set the variable's value to 'undefined' in the declaration and set its value exactly once after declaration if you want an immutable variable decided, for example, by a condition.";
			resolved_files.at(file_id).diagnostics.push_back(
				Diagnostic::error(
					"tried to mutate immutable variable",
					subtitle.str(),
					{Diagnostic::Sample(
						 get_context(symbol.file_id),
						 "declaration",
						 {Diagnostic::Sample::Label(symbol.span, OutFmt::Color::Cyan)}
					 ),
			                 Diagnostic::Sample(
						 get_context(file_id),
						 "mutation",
						 {Diagnostic::Sample::Label(set.name.span, OutFmt::Color::Red)}
					 )}
				)
			);
		}
		assigned.at(set.name.value) = true;
	}
	// TODO: fix this
	else
		std::cout
			<< "warning: setting undeclared "
			<< symbols.at(set.name.value).name
			<< " (@"
			<< set.name.value
			<< "); this shouldn't happen unless we somehow skip a variable declaration"
			<< std::endl;
}

void Analyzer::check_assigned(
	IR::Statement::Write&                     write,
	FileContext::ID                           file_id,
	std::unordered_map<IR::Identifier, bool>& assigned
) {
	// the pointer we're writing to must be assigned
	// TODO: move mutability check here
	check_assigned(write.address, file_id, assigned);
	check_assigned(write.value, file_id, assigned);
}

void Analyzer::check_assigned(
	IR::Statement::WriteAccess&               write_access,
	FileContext::ID                           file_id,
	std::unordered_map<IR::Identifier, bool>& assigned
) {
	// we do not support partial initialization, so the struct must be set
	// TODO: check mutability
	check_assigned(write_access.access.value, file_id, assigned);
	check_assigned(write_access.value, file_id, assigned);
}

void Analyzer::check_assigned(
	IR::Statement&                            statement,
	FileContext::ID                           file_id,
	std::unordered_map<IR::Identifier, bool>& assigned
) {
	switch (statement.kind()) {
	case IR::Statement::Kind::Declare:     return check_assigned(statement.get_declare(), file_id, assigned);
	case IR::Statement::Kind::Set:         return check_assigned(statement.get_set(), file_id, assigned);
	case IR::Statement::Kind::Call:        return check_assigned(statement.get_call(), file_id, assigned);
	case IR::Statement::Kind::Write:       return check_assigned(statement.get_write(), file_id, assigned);
	case IR::Statement::Kind::WriteAccess: return check_assigned(statement.get_write_access(), file_id, assigned);
	}
}

void Analyzer::check_assigned(
	IR::BasicBlock&                                                                 basic_block,
	IR::Function&                                                                   function,
	FileContext::ID                                                                 file_id,
	std::unordered_map<IR::Identifier, bool>&                                       assigned,
	std::unordered_map<IR::BasicBlock::ID, std::unordered_set<IR::BasicBlock::ID>>& preds
) {
	// first, we check every statement
	for (Spanned<IR::Statement>& statement : basic_block.statements)
		check_assigned(statement.value, file_id, assigned);

	// finally, we need to check how this fares in each of our possible jumps out
	// FIXME: this logic breaks in the file 'assigned'
	if (std::holds_alternative<IR::BasicBlock::Goto>(basic_block.jump)) {
		IR::BasicBlock::ID destination = std::get<IR::BasicBlock::Goto>(basic_block.jump).id;
		// we don't want to check a basic block we've already checked from this basic block
		if (preds.at(destination).contains(basic_block.id)) return;
		preds.at(destination).insert(basic_block.id);
		return check_assigned(function.body.at(destination), function, file_id, assigned, preds);
	} else if (std::holds_alternative<IR::BasicBlock::Branch>(basic_block.jump)) {
		IR::BasicBlock::Branch& branch = std::get<IR::BasicBlock::Branch>(basic_block.jump);
		// check the value
		check_assigned(branch.condition, file_id, assigned);
		// check both branches
		if (!preds.at(branch.true_).contains(basic_block.id)) {
			preds.at(branch.true_).insert(basic_block.id);
			// we copy to avoid info leak from the true branch onto the false branch
			std::unordered_map<IR::Identifier, bool> assigned_copy = assigned;
			check_assigned(function.body.at(branch.true_), function, file_id, assigned_copy, preds);
		}
		if (!preds.at(branch.false_).contains(basic_block.id)) {
			preds.at(branch.false_).insert(basic_block.id);
			check_assigned(function.body.at(branch.false_), function, file_id, assigned, preds);
		}
		return;
	} else if (std::holds_alternative<IR::BasicBlock::Return>(basic_block.jump)) {
		// this doesn't jump anywhere but we still have to check the value
		auto const& return_ = std::get<IR::BasicBlock::Return>(basic_block.jump);
		if (!return_.value.has_value()) return;
		check_assigned(return_.value.value(), file_id, assigned);
	}
}

void Analyzer::check_assigned(IR::Function& function, FileContext::ID file_id) {
	if (function.body.empty()) return;
	std::unordered_map<IR::Identifier, bool>                                       assigned {};
	std::unordered_map<IR::BasicBlock::ID, std::unordered_set<IR::BasicBlock::ID>> preds {};
	for (IR::BasicBlock::ID id = 0; id < function.body.size(); ++id) preds.insert({id, {}});
	check_assigned(function.body.at(0), function, file_id, assigned, preds);
}

void Analyzer::check_assigned(IR::Module& module, FileContext::ID file_id) {
	for (IR::Identifier item : module.items) {
		auto& value = symbols.at(item).item;
		if (std::holds_alternative<IR::Module>(value)) {
			check_assigned(std::get<IR::Module>(value), file_id);
		} else if (std::holds_alternative<IR::Function>(value)) {
			check_assigned(std::get<IR::Function>(value), file_id);
		}
	}
}

void Analyzer::check_assigned() {
	for (ResolvedFile& file : resolved_files) check_assigned(file.module, file.file_id);
}
