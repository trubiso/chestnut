#include "analyzer.hpp"

#include <sstream>
#include <variant>

void Analyzer::analyze(bool print_ir) {
	check_assigned();
	if (print_ir)
		for (ResolvedFile const& file : resolved_files) print(std::cout, file.module) << std::endl;
}

std::ostream& Analyzer::print(std::ostream& os, IR::Module const& module) const {
	os << "declare module @" << module.name.value << ": ";
	if (module.items.empty()) { return os << "(empty module)"; }
	os << "{\n";
	os.iword(0)++;
	for (auto item : module.items) {
		for (long i = 0; i < os.iword(0); ++i) os << "    ";
		if (std::holds_alternative<IR::Module>(symbols.at(item).item))
			print(os, std::get<IR::Module>(symbols.at(item).item));
		else if (std::holds_alternative<IR::Function>(symbols.at(item).item))
			os << std::get<IR::Function>(symbols.at(item).item);
		else if (std::holds_alternative<IR::Struct>(symbols.at(item).item))
			os << std::get<IR::Struct>(symbols.at(item).item);
		os << '\n';
	}
	os.iword(0)--;
	for (long i = 0; i < os.iword(0); ++i) os << "    ";
	return os << "}";
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
	IR::Identifier     identifier,
	Span               span,
	FileContext::ID    file_id,
	AssignedMap const& assigned
) {
	if (!assigned.contains(identifier)) return;
	if (!assigned.at(identifier))
		resolved_files.at(file_id).diagnostics.push_back(
			Diagnostic::error(
				"used potentially uninitialized variable",
				{Diagnostic::Sample(
					 get_context(symbols.at(identifier).file_id),
					 "declaration",
					 {Diagnostic::Sample::Label(symbols.at(identifier).span, OutFmt::Color::Cyan)}
				 ),
		                 Diagnostic::Sample(
					 get_context(file_id),
					 "usage",
					 {Diagnostic::Sample::Label(span, OutFmt::Color::Red)}
				 )}
			)
		);
}

void Analyzer::check_assigned(
	Spanned<IR::Identifier> const& identifier,
	FileContext::ID                file_id,
	AssignedMap const&             assigned
) {
	return check_assigned(identifier.value, identifier.span, file_id, assigned);
}

void Analyzer::check_assigned(
	IR::Expression::Atom const& atom,
	Span                        span,
	FileContext::ID             file_id,
	AssignedMap const&          assigned
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
	IR::Expression::FunctionCall const& function_call,
	FileContext::ID                     file_id,
	AssignedMap const&                  assigned
) {
	if (std::holds_alternative<Spanned<IR::Identifier>>(function_call.callee))
		check_assigned(std::get<Spanned<IR::Identifier>>(function_call.callee), file_id, assigned);
	for (auto const& argument : function_call.arguments) check_assigned(argument, file_id, assigned);
}

void Analyzer::check_assigned(
	IR::Expression::Deref const& deref,
	FileContext::ID              file_id,
	AssignedMap const&           assigned
) {
	check_assigned(deref.address, file_id, assigned);
}

void Analyzer::check_assigned(IR::Expression::Ref const& ref, FileContext::ID file_id, AssignedMap const& assigned) {
	check_assigned(ref.value, file_id, assigned);
}

void Analyzer::check_assigned(
	IR::Expression::MemberAccess const& member_access,
	FileContext::ID                     file_id,
	AssignedMap const&                  assigned
) {
	check_assigned(member_access.accessee, file_id, assigned);
}

void Analyzer::check_assigned(
	Spanned<IR::Expression::Atom> const& atom,
	FileContext::ID                      file_id,
	AssignedMap const&                   assigned
) {
	return check_assigned(atom.value, atom.span, file_id, assigned);
}

void Analyzer::check_assigned(
	Spanned<IR::Expression> const& expression,
	FileContext::ID                file_id,
	AssignedMap const&             assigned
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

void Analyzer::check_assigned(IR::Statement::Declare& declare, FileContext::ID file_id, AssignedMap& assigned) {
	assigned.insert({declare.name.value, {}});
}

void Analyzer::check_assigned(IR::Statement::Set& set, FileContext::ID file_id, AssignedMap& assigned) {
	check_assigned(set.value, file_id, assigned);
	if (assigned.contains(set.name.value)) {
		if (assigned.at(set.name.value).has_value() && !symbols.at(set.name.value).mutable_) {
			// mutability violation
			bool declare_and_set_is_same = symbols.at(set.name.value).span == assigned.at(set.name.value);
			IR::Symbol const& symbol     = symbols.at(set.name.value);
			std::stringstream subtitle {};
			subtitle << "variable '" << symbol.name << "' was declared as constant";
			if (declare_and_set_is_same)
				subtitle
					<< ".\ntip: you can set the variable's value to 'undefined' in the declaration and set its value exactly once after declaration if you want an immutable variable decided, for example, by a condition.";
			else
				// if they're not the same, the variable must have originally been undefined
				subtitle
					<< ", set to undefined and given a value after its declaration; but it was modified again afterwards.";
			std::vector<Diagnostic::Sample> samples {};
			samples.push_back(
				Diagnostic::Sample(
					get_context(symbol.file_id),
					"declaration",
					{Diagnostic::Sample::Label(symbol.span, OutFmt::Color::Cyan)}
				)
			);
			if (!declare_and_set_is_same)
				samples.push_back(
					Diagnostic::Sample(
						get_context(symbol.file_id),
						"value",
						{Diagnostic::Sample::Label(
							assigned.at(set.name.value).value(),
							OutFmt::Color::Magenta
						)}
					)
				);
			samples.push_back(
				Diagnostic::Sample(
					get_context(file_id),
					"mutation",
					{Diagnostic::Sample::Label(set.name.span, OutFmt::Color::Red)}
				)
			);
			resolved_files.at(file_id).diagnostics.push_back(
				Diagnostic::error(
					"tried to mutate immutable variable",
					subtitle.str(),
					std::move(samples)
				)
			);
		}
		assigned.at(set.name.value) = set.name.span;
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

void Analyzer::check_assigned(IR::Statement::Write& write, FileContext::ID file_id, AssignedMap& assigned) {
	// the pointer we're writing to must be assigned
	// TODO: move mutability check here
	check_assigned(write.address, file_id, assigned);
	check_assigned(write.value, file_id, assigned);
}

void Analyzer::check_assigned(
	IR::Statement::WriteAccess& write_access,
	FileContext::ID             file_id,
	AssignedMap&                assigned
) {
	// we do not support partial initialization, so the struct must be set
	// TODO: check mutability
	check_assigned(write_access.access.value, file_id, assigned);
	check_assigned(write_access.value, file_id, assigned);
}

void Analyzer::check_assigned(IR::Statement& statement, FileContext::ID file_id, AssignedMap& assigned) {
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
	AssignedMap&                                                                    assigned,
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
			AssignedMap assigned_copy = assigned;
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
	AssignedMap assigned {};

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
