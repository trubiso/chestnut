#include "analyzer.hpp"

#include <sstream>
#include <variant>

void Analyzer::analyze(bool print_ir) {
	optimize_blocks();
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

void Analyzer::optimize_blocks(IR::Function& function) {
	if (function.body.empty()) return;

	// optimize forwarding blocks
	std::unordered_map<IR::BasicBlock::ID, IR::BasicBlock::ID> forwarding {};
	// first, we check for all forwarding blocks
	for (IR::BasicBlock& basic_block : function.body) {
		if (basic_block.statements.empty() && std::holds_alternative<IR::BasicBlock::Goto>(basic_block.jump)) {
			forwarding.insert({basic_block.id, std::get<IR::BasicBlock::Goto>(basic_block.jump).id});
		}
	}
	// then, we replace their usage
	for (IR::BasicBlock& basic_block : function.body) {
		// we skip forwarding blocks themselves
		if (forwarding.contains(basic_block.id)) continue;
		if (std::holds_alternative<IR::BasicBlock::Goto>(basic_block.jump)) {
			IR::BasicBlock::Goto& goto_ = std::get<IR::BasicBlock::Goto>(basic_block.jump);
			// jump all references
			while (forwarding.contains(goto_.id)) goto_.id = forwarding.at(goto_.id);
		} else if (std::holds_alternative<IR::BasicBlock::Branch>(basic_block.jump)) {
			IR::BasicBlock::Branch& branch = std::get<IR::BasicBlock::Branch>(basic_block.jump);
			// jump all references
			while (forwarding.contains(branch.true_)) branch.true_ = forwarding.at(branch.true_);
			while (forwarding.contains(branch.false_)) branch.false_ = forwarding.at(branch.false_);
		}
	}

	// optimize unconditional jumps to empty blocks
	for (IR::BasicBlock& basic_block : function.body) {
		if (std::holds_alternative<IR::BasicBlock::Goto>(basic_block.jump)) {
			IR::BasicBlock::Goto& goto_     = std::get<IR::BasicBlock::Goto>(basic_block.jump);
			IR::BasicBlock const* jumped_to = &function.find_block(goto_.id);
			while (jumped_to->statements.empty()) {
				auto const& jump = jumped_to->jump;
				if (std::holds_alternative<IR::BasicBlock::Goto>(jump)) {
					goto_.id  = std::get<IR::BasicBlock::Goto>(jump).id;
					jumped_to = &function.find_block(goto_.id);
				} else if (std::holds_alternative<IR::BasicBlock::Branch>(jump)) {
					auto const& branch = std::get<IR::BasicBlock::Branch>(jump);
					basic_block.jump   = IR::BasicBlock::Branch {
						  {branch.condition.span, {branch.condition.value.clone()}},
                                                branch.true_,
                                                branch.false_,
                                        };
					break;
				} else if (std::holds_alternative<IR::BasicBlock::Return>(jump)) {
					basic_block.jump = IR::BasicBlock::Return {
						std::get<IR::BasicBlock::Return>(jump).value.transform(
							[](Spanned<IR::Expression::Atom> const& value) {
								return Spanned {value.span, value.value.clone()};
							}
						)
					};
					break;
				} else if (std::holds_alternative<std::monostate>(jump)) {
					basic_block.jump = std::monostate {};
					break;
				}
			}
		}
	}

	// TODO: remove basic blocks with no predecessors
}

void Analyzer::optimize_blocks(IR::Module& module) {
	for (IR::Identifier item : module.items) {
		auto& value = symbols.at(item).item;
		if (std::holds_alternative<IR::Module>(value)) {
			optimize_blocks(std::get<IR::Module>(value));
		} else if (std::holds_alternative<IR::Function>(value)) {
			optimize_blocks(std::get<IR::Function>(value));
		}
	}
}

void Analyzer::optimize_blocks() {
	for (ResolvedFile& file : resolved_files) optimize_blocks(file.module);
}

std::optional<IR::Identifier> Analyzer::get_base(IR::Place const& place) {
	switch (place.kind()) {
	case IR::Place::Kind::Symbol: return place.get_symbol();
	case IR::Place::Kind::Deref:  return get_base(place.get_deref().address->value);
	case IR::Place::Kind::Access: return get_base(place.get_access().accessee->value);
	case IR::Place::Kind::Error:  return std::nullopt;
	}
}

bool Analyzer::get_mutable(IR::Place const& place) {
	switch (place.kind()) {
	case IR::Place::Kind::Symbol: return symbols.at(place.get_symbol()).mutable_;
	case IR::Place::Kind::Deref:  break;
	case IR::Place::Kind::Access: return get_mutable(place.get_access().accessee->value);
	case IR::Place::Kind::Error:  return true;
	}

	// for deref, we must check the pointer itself
	IR::Type const& type = place.get_deref().address->value.type;
	if (!type.is_pointer()) return true;
	return type.get_pointer().mutable_;
}

Spanned<IR::Place> const* Analyzer::get_immutability_culprit(Spanned<IR::Place> const& place) {
	switch (place.value.kind()) {
	case IR::Place::Kind::Symbol:
		return &place;  // there are no subitems so we can safely assume this is the culprit
	case IR::Place::Kind::Deref:  return &place;  // since get_mutable stops at the first pointer, we can do so too!
	case IR::Place::Kind::Access: return get_immutability_culprit(*place.value.get_access().accessee);
	case IR::Place::Kind::Error:  return nullptr;
	}
}

void Analyzer::check_assigned(
	IR::Identifier     identifier,
	Span               span,
	FileContext::ID    file_id,
	AssignedMap const& assigned
) {
	// FIXME: if it is not contained but it is from the scope/function, that should throw a diagnostic, because we
	// are using a variable whose declaration we skipped over
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

void Analyzer::check_assigned(Spanned<IR::Place> const& place, FileContext::ID file_id, AssignedMap const& assigned) {
	switch (place.value.kind()) {
	case IR::Place::Kind::Symbol: return check_assigned(place.value.get_symbol(), place.span, file_id, assigned);
	case IR::Place::Kind::Deref:  return check_assigned(*place.value.get_deref().address, file_id, assigned);
	case IR::Place::Kind::Access: return check_assigned(*place.value.get_access().accessee, file_id, assigned);
	case IR::Place::Kind::Error:  return;
	}
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
	IR::Expression::Ref const& ref,
	Span                       span,
	FileContext::ID            file_id,
	AssignedMap const&         assigned
) {
	check_assigned(ref.value, file_id, assigned);
	std::optional<IR::Identifier> maybe_base = get_base(ref.value.value);
	if (!maybe_base.has_value()) return;
	IR::Identifier base = maybe_base.value();
	// TODO: throw diagnostic for references to dereferences (they don't do anything)
	if (!get_mutable(ref.value.value) && ref.mutable_) {
		Diagnostic::Sample reference_sample(
			get_context(file_id),
			"reference",
			{Diagnostic::Sample::Label(span, OutFmt::Color::Red)}
		);
		if (ref.value.value.is_symbol()) {
			std::stringstream subtitle {};
			IR::Symbol const& symbol = symbols.at(base);
			subtitle
				<< "symbol `"
				<< symbol.name
				<< "` was declared as constant, so a mutable reference cannot be taken";
			resolved_files.at(file_id).diagnostics.push_back(
				Diagnostic::error(
					"tried to take mutable reference of immutable symbol",
					subtitle.str(),
					{Diagnostic::Sample(
						 get_context(symbol.file_id),
						 "declaration",
						 {Diagnostic::Sample::Label(symbol.span, OutFmt::Color::Cyan)}
					 ),
			                 std::move(reference_sample)}
				)
			);
		} else if (ref.value.value.is_deref()) {
			resolved_files.at(file_id).diagnostics.push_back(
				Diagnostic::error(
					"tried to take mutable reference of dereferenced immutable pointer",
					"the value inside of a pointer declared as `*const` is not mutable, so a mutable reference cannot be taken",
					{std::move(reference_sample)}
				)
			);
		} else if (ref.value.value.is_access()) {
			Spanned<IR::Place> const&       culprit = *get_immutability_culprit(ref.value);
			std::stringstream               subtitle {};
			std::vector<Diagnostic::Sample> samples {};
			if (culprit.value.is_symbol()) {
				IR::Symbol const& symbol = symbols.at(culprit.value.get_symbol());
				subtitle
					<< "variable `"
					<< symbol.name
					<< "` must be declared as `mut` for its fields to allow mutation and, therefore, a mutable reference to be taken";
				samples.push_back(
					Diagnostic::Sample(
						get_context(symbol.file_id),
						"declaration",
						{Diagnostic::Sample::Label(symbol.span, OutFmt::Color::Cyan)}
					)
				);
			}
			samples.push_back(std::move(reference_sample));
			if (culprit.value.is_deref()) {
				subtitle
					<< "dereferenced pointer must be declared as `*mut` for its fields to allow mutation and, therefore, a mutable reference to be taken";
				samples.at(0).labels.at(0).span.start = culprit.span.end;
				samples.at(0).labels.push_back(
					Diagnostic::Sample::Label(culprit.span, "dereference", OutFmt::Color::Cyan)
				);
			}
			resolved_files.at(file_id).diagnostics.push_back(
				Diagnostic::error(
					"tried to take mutable reference of immutable field",
					subtitle.str(),
					std::move(samples)
				)
			);
		}
	}
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
	case IR::Expression::Kind::Ref:
		return check_assigned(expression.value.get_ref(), expression.span, file_id, assigned);
	case IR::Expression::Kind::Load: return check_assigned(expression.value.get_load().value, file_id, assigned);
	}
}

void Analyzer::check_assigned(IR::Statement::Declare& declare, FileContext::ID file_id, AssignedMap& assigned) {
	// we must replace the value, so if we go through a declaration more than once, it removes the value
	assigned.insert_or_assign(declare.name.value, std::nullopt);
}

void Analyzer::check_assigned(IR::Statement::Set& set, FileContext::ID file_id, AssignedMap& assigned) {
	check_assigned(set.value, file_id, assigned);
	std::optional<IR::Identifier> maybe_base = get_base(set.place.value);
	if (!maybe_base.has_value()) return;
	IR::Identifier base = maybe_base.value();
	if (assigned.contains(base)) {
		if (assigned.at(base).has_value() && !get_mutable(set.place.value)) {
			// mutability violation! time to throw the correct diagnostic
			Diagnostic::Sample mutation_sample(
				get_context(file_id),
				"mutation",
				{Diagnostic::Sample::Label(set.place.span, OutFmt::Color::Red)}
			);

			if (set.place.value.is_symbol()) {
				bool              declare_and_set_is_same = symbols.at(base).span == assigned.at(base);
				IR::Symbol const& symbol                  = symbols.at(base);
				std::stringstream subtitle {};
				subtitle << "variable `" << symbol.name << "` was declared as constant";
				if (declare_and_set_is_same)
					subtitle
						<< ".\ntip: you can set the variable's value to `undefined` in the declaration and set its value exactly once after declaration if you want an immutable variable decided, for example, by a condition.";
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
								assigned.at(base).value(),
								OutFmt::Color::Magenta
							)}
						)
					);
				samples.push_back(std::move(mutation_sample));
				resolved_files.at(file_id).diagnostics.push_back(
					Diagnostic::error(
						"tried to mutate immutable variable",
						subtitle.str(),
						std::move(samples)
					)
				);
			} else if (set.place.value.is_deref()) {
				resolved_files.at(file_id).diagnostics.push_back(
					Diagnostic::error(
						"tried to mutate immutable pointer",
						"the value inside of a pointer declared as `*const` cannot be mutated, only accessed. for mutation, you need a pointer declared as `*mut`",
						{std::move(mutation_sample)}
					)
				);
			} else if (set.place.value.is_access()) {
				Spanned<IR::Place> const&       culprit = *get_immutability_culprit(set.place);
				std::stringstream               subtitle {};
				std::vector<Diagnostic::Sample> samples {};
				if (culprit.value.is_symbol()) {
					IR::Symbol const& symbol = symbols.at(culprit.value.get_symbol());
					subtitle
						<< "variable `"
						<< symbol.name
						<< "` must be declared as `mut` for its fields to allow mutation";
					samples.push_back(
						Diagnostic::Sample(
							get_context(symbol.file_id),
							"declaration",
							{Diagnostic::Sample::Label(symbol.span, OutFmt::Color::Cyan)}
						)
					);
				}
				samples.push_back(std::move(mutation_sample));
				if (culprit.value.is_deref()) {
					subtitle
						<< "dereferenced pointer must be declared as `*mut` for its fields to allow mutation";
					samples.at(0).labels.at(0).span.start = culprit.span.end;
					samples.at(0).labels.push_back(
						Diagnostic::Sample::Label(
							culprit.span,
							"dereference",
							OutFmt::Color::Cyan
						)
					);
				}
				resolved_files.at(file_id).diagnostics.push_back(
					Diagnostic::error(
						"tried to mutate immutable field",
						subtitle.str(),
						std::move(samples)
					)
				);
			}
		} else assigned.at(base) = set.place.span;
	} else {
		resolved_files.at(file_id).diagnostics.push_back(
			Diagnostic::error(
				"tried to mutate undeclared variable",
				"this variable is declared in this scope, but that declaration does not occur before the variable is set, because it was skipped using low-level control flow constructs, i.e. goto/branch",
				{Diagnostic::Sample(get_context(file_id), set.place.span, OutFmt::Color::Red)}
			)
		);
	}
}

void Analyzer::check_assigned(IR::Statement& statement, Span span, FileContext::ID file_id, AssignedMap& assigned) {
	switch (statement.kind()) {
	case IR::Statement::Kind::Declare: return check_assigned(statement.get_declare(), file_id, assigned);
	case IR::Statement::Kind::Set:     return check_assigned(statement.get_set(), file_id, assigned);
	case IR::Statement::Kind::Call:    return check_assigned(statement.get_call(), file_id, assigned);
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
		check_assigned(statement.value, statement.span, file_id, assigned);

	// finally, we need to check how this fares in each of our possible jumps out
	if (std::holds_alternative<IR::BasicBlock::Goto>(basic_block.jump)) {
		IR::BasicBlock::ID destination = std::get<IR::BasicBlock::Goto>(basic_block.jump).id;
		// we don't want to check a basic block we've already checked from this basic block
		if (preds.at(destination).contains(basic_block.id)) return;
		preds.at(destination).insert(basic_block.id);
		return check_assigned(function.find_block(destination), function, file_id, assigned, preds);
	} else if (std::holds_alternative<IR::BasicBlock::Branch>(basic_block.jump)) {
		IR::BasicBlock::Branch& branch = std::get<IR::BasicBlock::Branch>(basic_block.jump);
		// check the value
		check_assigned(branch.condition, file_id, assigned);
		// check both branches
		if (!preds.at(branch.true_).contains(basic_block.id)) {
			preds.at(branch.true_).insert(basic_block.id);
			// we copy to avoid info leak from the true branch onto the false branch
			AssignedMap assigned_copy = assigned;
			check_assigned(function.find_block(branch.true_), function, file_id, assigned_copy, preds);
		}
		if (!preds.at(branch.false_).contains(basic_block.id)) {
			preds.at(branch.false_).insert(basic_block.id);
			check_assigned(function.find_block(branch.false_), function, file_id, assigned, preds);
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
