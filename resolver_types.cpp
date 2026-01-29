#include "levenshtein.hpp"
#include "resolver.hpp"

#include <cstdlib>
#include <iostream>
#include <variant>

Resolver::TypeInfo Resolver::TypeInfo::from_type(AST::Type::Atom const& atom) {
	switch (atom.kind()) {
	case AST::Type::Atom::Kind::Float:    return make_known_float(KnownFloat {atom.get_float().width});
	case AST::Type::Atom::Kind::Void:     return make_known_void();
	case AST::Type::Atom::Kind::Char:     return make_known_char();
	case AST::Type::Atom::Kind::Bool:     return make_known_bool();
	case AST::Type::Atom::Kind::Inferred: return make_unknown();
	case AST::Type::Atom::Kind::Integer:  break;
	}

	// for integers, we need to determine how much information we know
	AST::Type::Atom::Integer const& integer = atom.get_integer();
	if (integer.width_type() != AST::Type::Atom::Integer::WidthType::Any)
		return make_known_integer(KnownInteger {integer});
	else return make_partial_integer(PartialInteger {integer, true});
}

Resolver::TypeInfo Resolver::TypeInfo::from_type(AST::Type const& type) {
	switch (type.kind()) {
	case AST::Type::Kind::Atom: return from_type(type.get_atom());
	}
	[[assume(false)]];
}

bool Resolver::TypeInfo::is_callable(std::vector<Resolver::TypeInfo> const& pool) const {
	switch (kind()) {
	case Kind::Function: return true;
	case Kind::SameAs:
		for (TypeInfo::ID id : get_same_as().ids)
			if (pool.at(id).is_callable(pool)) return true;
		return false;
	default: return false;
	}
}

std::vector<Resolver::TypeInfo::ID> Resolver::TypeInfo::get_callable_subitems(
	Resolver::TypeInfo::ID                 self_id,
	std::vector<Resolver::TypeInfo> const& pool
) const {
	switch (kind()) {
	case Kind::Function: return {self_id};
	case Kind::SameAs:   break;
	default:             return {};
	}

	// for SameAs, we just collect all the children
	std::vector<ID> ids {};
	for (TypeInfo::ID id : get_same_as().ids) {
		std::vector<ID> subids = pool.at(id).get_callable_subitems(id, pool);
		for (TypeInfo::ID subid : subids) ids.push_back(subid);
	}

	return ids;
}

void Resolver::debug_print_type(Resolver::TypeInfo::ID id) const {
	std::cout << "$" << id << " ";
	debug_print_type(type_pool_.at(id));
}

void Resolver::debug_print_type(Resolver::TypeInfo type) const {
	switch (type.kind()) {
	case TypeInfo::Kind::Unknown:        std::cout << "(unknown type)"; return;
	case TypeInfo::Kind::Bottom:         std::cout << "(bottom)"; return;
	case TypeInfo::Kind::Module:         std::cout << "(module)"; return;
	case TypeInfo::Kind::KnownVoid:      std::cout << "void"; return;
	case TypeInfo::Kind::KnownChar:      std::cout << "char"; return;
	case TypeInfo::Kind::KnownBool:      std::cout << "bool"; return;
	case TypeInfo::Kind::PartialFloat:   std::cout << "(float)"; return;
	case TypeInfo::Kind::Number:         std::cout << "(numeric)"; return;
	case TypeInfo::Kind::Function:
	case TypeInfo::Kind::SameAs:
	case TypeInfo::Kind::KnownInteger:
	case TypeInfo::Kind::KnownFloat:
	case TypeInfo::Kind::PartialInteger: break;
	}

	if (type.kind() == TypeInfo::Kind::Function) {
		TypeInfo::Function const& function = type.get_function();
		std::cout << "(function with args (";
		size_t count = 0;
		for (auto const& [name, arg_type] : function.arguments) {
			std::cout << (name.has_value() ? name.value() : "(anonymous)") << ": ";
			debug_print_type(arg_type);
			if (++count < function.arguments.size()) std::cout << ", ";
		}
		std::cout << ") and return type ";
		debug_print_type(function.return_);
		std::cout << ")";
	} else if (type.kind() == TypeInfo::Kind::SameAs) {
		std::cout << "=(";
		size_t count = 0;
		for (Resolver::TypeInfo::ID subid : type.get_same_as().ids) {
			debug_print_type(subid);
			if (++count < type.get_same_as().ids.size()) std::cout << " | ";
		}
		std::cout << ")";
	} else if (type.kind() == TypeInfo::Kind::KnownInteger) {
		AST::Type::Atom::Integer integer = type.get_known_integer().integer;
		std::cout << AST::Type::Atom::make_integer(std::move(integer));
	} else if (type.kind() == TypeInfo::Kind::KnownFloat) {
		std::cout << AST::Type::Atom::make_float(type.get_known_float().width);
	} else if (type.kind() == TypeInfo::Kind::PartialInteger) {
		AST::Type::Atom::Integer integer         = type.get_partial_integer().integer;
		bool                     signed_is_known = type.get_partial_integer().signed_is_known;
		std::cout
			<< (signed_is_known ? "(unknown sign) " : "")
			<< AST::Type::Atom::make_integer(std::move(integer));
	}
}

Resolver::TypeInfo::ID Resolver::type_next() {
	return type_counter_++;
}

Resolver::TypeInfo::ID Resolver::register_type(Resolver::TypeInfo&& type) {
	Resolver::TypeInfo::ID id = type_next();
	assert(type_pool_.size() == id);
	type_pool_.push_back(std::move(type));
	return id;
}

Resolver::TypeInfo
Resolver::follow_references(Resolver::TypeInfo::ID same_as, Resolver::TypeInfo::ID other, FileContext::ID file_id) {
	std::vector<TypeInfo::ID> const& ids = type_pool_.at(same_as).get_same_as().ids;
	// if we have a single id, unify it as normal
	if (ids.size() == 1) {
		unify(ids[0], other, file_id);
		return TypeInfo::make_same_as(ids[0]);
	}

	// if we have more than one, filter the non-unifiable ones out
	std::vector<TypeInfo::ID> new_ids {};
	for (TypeInfo::ID id : ids)
		if (can_unify(id, other)) new_ids.push_back(id);

	// if none can be unified, throw a diagnostic and return Bottom
	if (new_ids.empty()) {
		// TODO: diagnostic
		std::cout << "none of these ids can unify with anything from the other side" << std::endl;
		return TypeInfo::make_bottom();
	}

	// unify all remaining ids and create a new SameAs
	for (TypeInfo::ID new_id : new_ids) unify(new_id, other, file_id);
	return TypeInfo::make_same_as(std::move(new_ids));
}

bool Resolver::basic_known(
	Resolver::TypeInfo::Kind kind,
	Resolver::TypeInfo::ID   a_id,
	Resolver::TypeInfo::ID   b_id,
	FileContext::ID          file_id
) {
	TypeInfo &a = type_pool_.at(a_id), &b = type_pool_.at(b_id);
	bool      a_matches = a.kind() == kind, b_matches = b.kind() == kind;
	if (!a_matches && !b_matches) return false;  // if neither match, this case is not for us
	if (a_matches && b_matches) return true;     // if both match, this is a freebie
	// whichever didn't match becomes a bottom
	if (!a_matches) a = TypeInfo::make_bottom();
	if (!b_matches) b = TypeInfo::make_bottom();
	// we add a diagnostic
	// TODO: diagnostic
	std::cout << "found basic known type mismatch" << std::endl;
	return true;
}

void Resolver::unify(Resolver::TypeInfo::ID a_id, Resolver::TypeInfo::ID b_id, FileContext::ID file_id) {
	TypeInfo &a = type_pool_.at(a_id), &b = type_pool_.at(b_id);

	// follow references
	if (a.kind() == TypeInfo::Kind::SameAs) {
		a = follow_references(a_id, b_id, file_id);
		return;
	}
	if (b.kind() == TypeInfo::Kind::SameAs) {
		b = follow_references(b_id, a_id, file_id);
		return;
	}

	// bottoms don't participate in unification
	if (a.kind() == TypeInfo::Kind::Bottom || b.kind() == TypeInfo::Kind::Bottom) return;

	// make unknowns known
	if (a.kind() == TypeInfo::Kind::Unknown) {
		a = TypeInfo::make_same_as(b_id);
		return;
	}
	if (b.kind() == TypeInfo::Kind::Unknown) {
		b = TypeInfo::make_same_as(a_id);
		return;
	}

	// if any of them is a basic Known type, the other must be exactly the same
	if (basic_known(TypeInfo::Kind::KnownVoid, a_id, b_id, file_id)) return;
	if (basic_known(TypeInfo::Kind::KnownChar, a_id, b_id, file_id)) return;
	if (basic_known(TypeInfo::Kind::KnownBool, a_id, b_id, file_id)) return;

	// modules act like basic Known types right now, but this is silly.
	// TODO: do something better
	if (basic_known(TypeInfo::Kind::Module, a_id, b_id, file_id)) return;

	// TODO: Function
	// TODO: KnownInteger
	// TODO: KnownFloat
	// TODO: PartialInteger
	// TODO: PartialFloat
	// TODO: Number
	std::cout << "tried to unify ";
	debug_print_type(a_id);
	std::cout << " and ";
	debug_print_type(b_id);
	std::cout << std::endl;
}

bool Resolver::can_unify(Resolver::TypeInfo::ID a, Resolver::TypeInfo::ID b) {
	return can_unify(type_pool_.at(a), type_pool_.at(b));
}

bool Resolver::can_unify(Resolver::TypeInfo a, Resolver::TypeInfo::ID b) {
	return can_unify(a, type_pool_.at(b));
}

bool Resolver::can_unify(Resolver::TypeInfo a, Resolver::TypeInfo b) {
	std::cout << "checking unifiability between ";
	debug_print_type(a);
	std::cout << " and ";
	debug_print_type(b);
	std::cout << std::endl;
	return true;
}

Resolver::TypeInfo Resolver::infer(AST::Expression::Atom const& atom, FileContext::ID file_id) {
	switch (atom.kind()) {
	case AST::Expression::Atom::Kind::NumberLiteral:
		// TODO: apply suffixes
		return TypeInfo::make_number();
	case AST::Expression::Atom::Kind::StringLiteral:
		// TODO: do string literals
		std::cout << "unsupported string literal" << std::endl;
		std::exit(0);
	case AST::Expression::Atom::Kind::CharLiteral:
		// TODO: apply suffixes
		return TypeInfo::make_known_char();
	case AST::Expression::Atom::Kind::Expression: return infer(*atom.get_expression(), file_id);
	case AST::Expression::Atom::Kind::Identifier: break;
	}

	// for identifiers, we match the type in the symbol pool
	AST::Identifier const& identifier = atom.get_identifier();
	if (!identifier.id.has_value()) return TypeInfo::make_bottom();  // if we don't know what this is, let's ignore
	std::vector<AST::SymbolID> const& ids = identifier.id.value();
	if (ids.empty()) return TypeInfo::make_bottom();
	std::vector<TypeInfo::ID> type_ids {};
	type_ids.reserve(ids.size());
	for (AST::SymbolID id : ids) { type_ids.push_back(get_single_symbol(id).type); }
	if (type_ids.size() == 1) return TypeInfo::make_same_as(type_ids[0]);
	else return TypeInfo::make_same_as(std::move(type_ids));
}

Resolver::TypeInfo Resolver::infer(AST::Expression::UnaryOperation const& unary_operation, FileContext::ID file_id) {
	// TODO: operators
	TypeInfo operand = infer(unary_operation.operand->value, file_id);
	return operand;
}

Resolver::TypeInfo Resolver::infer(AST::Expression::BinaryOperation const& binary_operation, FileContext::ID file_id) {
	// TODO: operators
	TypeInfo lhs = infer(binary_operation.lhs->value, file_id);
	TypeInfo rhs = infer(binary_operation.rhs->value, file_id);

	TypeInfo::ID lhs_id = register_type(std::move(lhs));
	TypeInfo::ID rhs_id = register_type(std::move(rhs));
	unify(lhs_id, rhs_id, file_id);

	return TypeInfo::make_same_as(lhs_id);
}

Resolver::TypeInfo Resolver::infer(AST::Expression::FunctionCall const& function_call, FileContext::ID file_id) {
	// for function calls, we need to resolve or partially resolve the overload
	TypeInfo callee = infer(function_call.callee->value, file_id);

	TypeInfo::ID callee_id = register_type(std::move(callee));

	// first, we ensure that there is at least one callable item
	if (!type_pool_.at(callee_id).is_callable(type_pool_)) {
		// TODO: proper diagnostic
		std::cout << "called a non-function loool" << std::endl;
		return TypeInfo::make_bottom();
	}

	// now, we flatten this type to a SameAs which points at all the callable types
	std::vector<TypeInfo::ID> callable = type_pool_.at(callee_id).get_callable_subitems(callee_id, type_pool_);
	assert(!callable.empty());  // we know is_callable()

	// we need to further filter this to ensure we have same argument count and our labeled arguments match
	// TODO: this works for now, but will break once arguments have default values
	size_t provided_arguments = function_call.arguments.labeled.size() + function_call.arguments.ordered.size();
	std::vector<TypeInfo::ID> callable_filtered {};
	for (TypeInfo::ID callable_id : callable) {
		assert(type_pool_.at(callable_id).kind() == TypeInfo::Kind::Function);
		auto const& function_arguments     = type_pool_.at(callable_id).get_function().arguments;
		bool        argument_count_matches = function_arguments.size() == provided_arguments;
		if (!argument_count_matches) continue;
		if (!function_call.arguments.labeled.empty()) {
			// check that all labeled arguments exist
			// we only consider arguments that haven't already been provided by the ordered arguments
			std::vector<std::string_view> arguments_under_consideration {};
			arguments_under_consideration.reserve(
				function_arguments.size() - function_call.arguments.ordered.size()
			);
			for (size_t i = function_call.arguments.ordered.size(); i < function_arguments.size(); ++i)
				if (std::get<0>(function_arguments.at(i)).has_value())
					arguments_under_consideration.push_back(
						std::get<0>(function_arguments.at(i)).value()
					);

			bool all_arguments_exist = true;
			for (auto const& argument : function_call.arguments.labeled) {
				auto const& identifier      = std::get<0>(argument);
				bool        argument_exists = false;
				for (std::string_view actual_argument : arguments_under_consideration) {
					if (actual_argument == identifier.value.name()) {
						argument_exists = true;
						break;
					}
				}
				if (!argument_exists) {
					all_arguments_exist = false;
					break;
				}
			}
			if (!all_arguments_exist) continue;
		}
		callable_filtered.push_back(callable_id);
	}

	if (callable_filtered.empty()) {
		// TODO: throw a really cool diagnostic showing exactly why each function was discarded
		std::cout << "no function matches " << function_call << " lol" << std::endl;
		return TypeInfo::make_bottom();
	}

	// it's time to infer the arguments now that we have a set of possible callees
	std::vector<TypeInfo::ID> ordered_arguments {};
	ordered_arguments.reserve(function_call.arguments.ordered.size());
	for (auto const& argument : function_call.arguments.ordered) {
		ordered_arguments.push_back(register_type(infer(argument.value, file_id)));
	}

	std::unordered_map<std::string, TypeInfo::ID> labeled_arguments {};
	ordered_arguments.reserve(function_call.arguments.labeled.size());
	for (auto const& [identifier, value] : function_call.arguments.labeled) {
		labeled_arguments.emplace(identifier.value.name(), register_type(infer(value.value, file_id)));
	}

	TypeInfo::ID return_ = register_type(TypeInfo::make_unknown());

	// we will filter the functions based on whether they are unifiable
	std::vector<TypeInfo::ID>   found_functions {};
	std::optional<TypeInfo::ID> call_id = {};
	for (TypeInfo::ID callable_id : callable_filtered) {
		assert(type_pool_.at(callable_id).kind() == TypeInfo::Kind::Function);
		// now we must create the function call type according to the function (due to labeled arguments)
		auto const& function_arguments = type_pool_.at(callable_id).get_function().arguments;
		assert(function_arguments.size() == provided_arguments);
		std::vector<std::tuple<std::optional<std::string>, TypeInfo::ID>> arguments {};
		arguments.reserve(provided_arguments);
		for (TypeInfo::ID argument : ordered_arguments) arguments.push_back({std::nullopt, argument});
		for (size_t i = arguments.size(); i < function_arguments.size(); ++i) {
			std::optional<std::string> name = std::get<0>(function_arguments.at(i));
			// if it didn't have a name, it couldn't be a labeled argument
			assert(name.has_value());
			arguments.push_back({name, labeled_arguments.at(name.value())});
		}

		// TODO: once we have generics, we need to instantiate a copy of the function with unknowns
		TypeInfo function_call_type
			= TypeInfo::make_function(TypeInfo::Function {std::move(arguments), return_});
		if (can_unify(function_call_type, callable_id)) {
			if (found_functions.empty()) {
				call_id = register_type(std::move(function_call_type));
				unify(call_id.value(), callable_id, file_id);
			}
			found_functions.push_back(callable_id);
		}
	}

	if (found_functions.empty() || !call_id.has_value()) {
		// TODO: throw a diagnostic
		std::cout << "no function matches after typeres lol" << std::endl;
		return TypeInfo::make_bottom();
	}

	if (found_functions.size() > 1) {
		std::cout << "too many functions match" << std::endl;
		return TypeInfo::make_bottom();
	}

	// TODO: finish resolving the identifier that is being called?

	return TypeInfo::make_same_as(type_pool_.at(call_id.value()).get_function().return_);
}

Resolver::TypeInfo Resolver::infer(AST::Expression const& expression, FileContext::ID file_id) {
	switch (expression.kind()) {
	case AST::Expression::Kind::Atom:            return infer(expression.get_atom(), file_id);
	case AST::Expression::Kind::UnaryOperation:  return infer(expression.get_unary_operation(), file_id);
	case AST::Expression::Kind::BinaryOperation: return infer(expression.get_binary_operation(), file_id);
	case AST::Expression::Kind::FunctionCall:    return infer(expression.get_function_call(), file_id);
	}
}

void Resolver::infer(AST::Statement::Declare& declare, FileContext::ID file_id) {
	if (!declare.value.has_value()) return;
	TypeInfo::ID variable_type = get_single_symbol(declare.name.value).type;
	TypeInfo     value         = infer(declare.value.value().value, file_id);
	TypeInfo::ID value_type    = register_type(std::move(value));
	// we must make sure that the declared and actual type match
	unify(variable_type, value_type, file_id);
}

void Resolver::infer(AST::Statement::Set& set, FileContext::ID file_id) {
	// skip all invalid LHS
	if (set.lhs.value.kind() != AST::Expression::Kind::Atom) {
		std::cout
			<< "unsupported set statement (this could also go for functions which, tbh, why can you even assign values to them)"
			<< std::endl;
		return;
	}
	if (set.lhs.value.get_atom().kind() != AST::Expression::Atom::Kind::Identifier) return;
}

void Resolver::infer(AST::Statement::Return& return_, AST::SymbolID function, FileContext::ID file_id) {
	TypeInfo     return_value    = return_.value.has_value()
                                      // if we do have a value, get its expression type
	                                     ? infer(return_.value.value().value, file_id)
                                      // if we don't, that's a return void
	                                     : TypeInfo::make_known_void();
	TypeInfo::ID return_value_id = register_type(std::move(return_value));
	// we must make sure that the return type and the returned value match
	unify(return_value_id, type_pool_.at(symbol_pool_.at(function).type).get_function().return_, file_id);
}

void Resolver::infer(Spanned<AST::Statement>& statement, AST::SymbolID function, FileContext::ID file_id) {
	switch (statement.value.kind()) {
	case AST::Statement::Kind::Declare: infer(statement.value.get_declare(), file_id); return;
	case AST::Statement::Kind::Set:     infer(statement.value.get_set(), file_id); return;
	case AST::Statement::Kind::Expression:
		infer(statement.value.get_expression(), file_id);  // TODO: should we unify this with void?
		return;
	case AST::Statement::Kind::Return: infer(statement.value.get_return(), function, file_id); return;
	case AST::Statement::Kind::Scope:  infer(statement.value.get_scope(), function, file_id); return;
	}
}

void Resolver::infer(AST::Scope& scope, AST::SymbolID function, FileContext::ID file_id) {
	for (auto& statement : scope) { infer(statement, function, file_id); }
}

void Resolver::infer(AST::Function& function, FileContext::ID file_id) {
	if (function.body.has_value()) infer(function.body.value(), function.name.value.id.value()[0], file_id);
}

void Resolver::infer(AST::Module& module, FileContext::ID file_id) {
	for (Spanned<AST::Module::Item>& item : module.body.items) {
		auto& value = std::get<AST::Module::InnerItem>(item.value);
		if (std::holds_alternative<AST::Function>(value)) infer(std::get<AST::Function>(value), file_id);
		else if (std::holds_alternative<AST::Module>(value)) infer(std::get<AST::Module>(value), file_id);
	}
}

void Resolver::infer_types() {
	for (ParsedFile& file : parsed_files) { infer(file.module, file.file_id); }
}
