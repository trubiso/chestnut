#include "ir.hpp"

#include <iostream>

namespace IR {

Type Type::clone() const {
	if (is_atom()) {
		Type::Atom atom = get_atom();
		return Type::make_atom(std::move(atom));
	}

	Type::Pointer const& pointer = get_pointer();
	return Type::make_pointer(
		Type::Pointer {
			std::make_unique<Spanned<Type>>(Spanned {pointer.type->span, pointer.type->value.clone()}),
			pointer.mutable_
		}
	);
}

std::ostream& operator<<(std::ostream& os, Type::Atom const& atom) {
	switch (atom.kind()) {
	case Type::Atom::Kind::Integer: break;
	case Type::Atom::Kind::Float:   return os << "float" << (uint32_t) atom.get_float().width_value();
	case Type::Atom::Kind::Void:    return os << "void";
	case Type::Atom::Kind::Char:    return os << "char";
	case Type::Atom::Kind::Bool:    return os << "bool";
	case Type::Atom::Kind::Named:   return os << '@' << atom.get_named();
	case Type::Atom::Kind::Error:   return os << "(error)";
	}

	// we know it's an integer now
	Type::Atom::Integer int_ = atom.get_integer();
	os << (int_.is_signed() ? "int" : "uint");
	Type::Atom::Integer::WidthType width_type = int_.width_type();

	switch (width_type) {
	case Type::Atom::Integer::WidthType::Fixed: return os << int_.bit_width().value();
	case Type::Atom::Integer::WidthType::Ptr:   return os << "ptr";
	case Type::Atom::Integer::WidthType::Size:  return os << "size";
	}
	[[assume(false)]];
}

std::ostream& operator<<(std::ostream& os, Type::Pointer const& pointer) {
	return os << "(*" << (pointer.mutable_ ? "mut" : "const") << ' ' << pointer.type->value << ")";
}

std::ostream& operator<<(std::ostream& os, Type const& type) {
	switch (type.kind()) {
	case Type::Kind::Atom:    return os << type.get_atom();
	case Type::Kind::Pointer: return os << type.get_pointer();
	}
	[[assume(false)]];
}

std::ostream& operator<<(std::ostream& os, BuiltInFunction const& built_in_function) {
	os << '%';
	switch (built_in_function) {
	case BuiltInFunction::AddUIntegers:      return os << "AddUIntegers";
	case BuiltInFunction::AddSIntegers:      return os << "AddSIntegers";
	case BuiltInFunction::AddFloats:         return os << "AddFloats";
	case BuiltInFunction::SubtractUIntegers: return os << "SubtractUIntegers";
	case BuiltInFunction::SubtractSIntegers: return os << "SubtractSIntegers";
	case BuiltInFunction::SubtractFloats:    return os << "SubtractFloats";
	case BuiltInFunction::MultiplyUIntegers: return os << "MultiplyUIntegers";
	case BuiltInFunction::MultiplySIntegers: return os << "MultiplySIntegers";
	case BuiltInFunction::MultiplyFloats:    return os << "MultiplyFloats";
	case BuiltInFunction::DivideUIntegers:   return os << "DivideUIntegers";
	case BuiltInFunction::DivideSIntegers:   return os << "DivideSIntegers";
	case BuiltInFunction::DivideFloats:      return os << "DivideFloats";
	case BuiltInFunction::NegateSInteger:    return os << "NegateSInteger";
	case BuiltInFunction::NegateFloat:       return os << "NegateFloat";
	case BuiltInFunction::NegateBool:        return os << "NegateBool";
	case BuiltInFunction::EqIntegers:        return os << "EqIntegers";
	case BuiltInFunction::EqFloats:          return os << "EqFloats";
	case BuiltInFunction::EqChars:           return os << "EqChars";
	case BuiltInFunction::EqBools:           return os << "EqBools";
	case BuiltInFunction::NeIntegers:        return os << "NeIntegers";
	case BuiltInFunction::NeFloats:          return os << "NeFloats";
	case BuiltInFunction::NeChars:           return os << "NeChars";
	case BuiltInFunction::NeBools:           return os << "NeBools";
	case BuiltInFunction::GtUIntegers:       return os << "GtUIntegers";
	case BuiltInFunction::GtSIntegers:       return os << "GtSIntegers";
	case BuiltInFunction::GtFloats:          return os << "GtFloats";
	case BuiltInFunction::GeUIntegers:       return os << "GeUIntegers";
	case BuiltInFunction::GeSIntegers:       return os << "GeSIntegers";
	case BuiltInFunction::GeFloats:          return os << "GeFloats";
	case BuiltInFunction::LtUIntegers:       return os << "LtUIntegers";
	case BuiltInFunction::LtSIntegers:       return os << "LtSIntegers";
	case BuiltInFunction::LtFloats:          return os << "LtFloats";
	case BuiltInFunction::LeUIntegers:       return os << "LeUIntegers";
	case BuiltInFunction::LeSIntegers:       return os << "LeSIntegers";
	case BuiltInFunction::LeFloats:          return os << "LeFloats";
	}
}

Expression::Atom Expression::Atom::clone() const {
	switch (kind()) {
	case Kind::Identifier:    return make_identifier(get_identifier(), type.clone());
	case Kind::Literal:       return {get_literal(), type.clone()};
	case Kind::Bool:          return make_bool(get_bool(), type.clone());
	case Kind::StructLiteral: break;
	case Kind::Error:         return make_error();
	}

	std::vector<Spanned<Atom>> fields {};
	fields.reserve(get_struct_literal().fields.size());
	std::transform(
		get_struct_literal().fields.cbegin(),
		get_struct_literal().fields.cend(),
		std::back_inserter(fields),
		[](Spanned<Atom> const& atom) { return Spanned {atom.span, atom.value.clone()}; }
	);
	return {
		StructLiteral {get_struct_literal().name, std::move(fields)},
		type.clone()
	};
}

std::ostream& operator<<(std::ostream& os, Expression::Atom::Literal const& literal) {
	switch (literal.kind) {
	case Expression::Atom::Literal::Kind::Number: os << "(number) "; break;
	case Expression::Atom::Literal::Kind::String: os << "(string) "; break;
	case Expression::Atom::Literal::Kind::Char:   os << "(char) "; break;
	}
	return os << literal.literal;
}

std::ostream& operator<<(std::ostream& os, Expression::Atom::StructLiteral const& struct_literal) {
	os << "[struct literal of type @" << struct_literal.name.value << " with ";
	if (struct_literal.fields.empty()) return os << "no fields]";
	os << "fields ";
	size_t count = 0;
	for (auto const& field : struct_literal.fields) {
		os << field.value;
		if (++count < struct_literal.fields.size()) os << ", ";
	}
	return os << ']';
}

std::ostream& operator<<(std::ostream& os, Expression::Atom const& atom) {
	switch (atom.kind()) {
	case Expression::Atom::Kind::Identifier:    return os << '@' << atom.get_identifier();
	case Expression::Atom::Kind::Literal:       return os << atom.get_literal();
	case Expression::Atom::Kind::Bool:          return os << "(bool) " << (atom.get_bool() ? "true" : "false");
	case Expression::Atom::Kind::StructLiteral: return os << atom.get_struct_literal();
	case Expression::Atom::Kind::Error:         return os << "(error)";
	}
	[[assume(false)]];
}

std::ostream& operator<<(std::ostream& os, Expression::FunctionCall const& call) {
	os << "(call ";
	if (std::holds_alternative<Spanned<Identifier>>(call.callee))
		os << '@' << std::get<Spanned<Identifier>>(call.callee).value;
	else os << std::get<BuiltInFunction>(call.callee);
	if (!call.arguments.empty()) {
		os << " w/ args: (";
		for (size_t i = 0; i < call.arguments.size(); ++i) {
			os << call.arguments[i].value;
			if (i + 1 < call.arguments.size()) os << ", ";
		}
		os << ')';
	}
	return os << ')';
}

std::ostream& operator<<(std::ostream& os, Expression::Deref const& deref) {
	return os << "(deref @" << deref.address.value << ')';
}

std::ostream& operator<<(std::ostream& os, Expression::Ref const& ref) {
	return os << "(&" << (ref.mutable_ ? "mut" : "const") << " (@" << ref.value.value << "))";
}

std::ostream& operator<<(std::ostream& os, Expression::MemberAccess const& member_access) {
	return os << "(@" << member_access.accessee.value << ".+" << member_access.field_index << ")";
}

std::ostream& operator<<(std::ostream& os, Expression const& expression) {
	switch (expression.kind()) {
	case Expression::Kind::Atom:         return os << expression.get_atom();
	case Expression::Kind::FunctionCall: return os << expression.get_function_call();
	case Expression::Kind::Deref:        return os << expression.get_deref();
	case Expression::Kind::Ref:          return os << expression.get_ref();
	case Expression::Kind::MemberAccess: return os << expression.get_member_access();
	}
	[[assume(false)]];
}

std::ostream& operator<<(std::ostream& os, Statement::Declare const& declare) {
	os << "[declare stmt: ";
	os << (declare.mutable_.value ? "mut" : "const") << " @" << declare.name.value;
	return os << ": " << declare.type << ";]";
}

std::ostream& operator<<(std::ostream& os, Statement::Set const& set) {
	os << "[set stmt: ";
	os << '@' << set.name.value << " = " << set.value.value;
	return os << ";]";
}

std::ostream& operator<<(std::ostream& os, Statement::Write const& write) {
	os << "[write stmt: ";
	os << "*@" << write.address.value << " = " << write.value.value;
	return os << ";]";
}

std::ostream& operator<<(std::ostream& os, Statement::WriteAccess const& write) {
	os << "[write access stmt: ";
	os << write.access.value << " = " << write.value.value;
	return os << ";]";
}

std::ostream& operator<<(std::ostream& os, Statement const& statement) {
	for (long i = 0; i < os.iword(0); ++i) os << "    ";
	switch (statement.kind()) {
	case Statement::Kind::Declare:     return os << statement.get_declare();
	case Statement::Kind::Set:         return os << statement.get_set();
	case Statement::Kind::Call:        return os << "[call stmt: " << statement.get_call() << ";]";
	case Statement::Kind::Write:       return os << statement.get_write();
	case Statement::Kind::WriteAccess: return os << statement.get_write_access();
	}
}

std::ostream& operator<<(std::ostream& os, BasicBlock const& basic_block) {
	os << "'@" << basic_block.id << " {\n";
	os.iword(0)++;
	for (auto const& stmt : basic_block.statements) { os << stmt.value << '\n'; }
	os.iword(0)--;
	for (long i = 0; i < os.iword(0); ++i) os << "    ";
	os << "}";
	if (std::holds_alternative<BasicBlock::Goto>(basic_block.jump))
		return os << " goto '@" << std::get<BasicBlock::Goto>(basic_block.jump).id;
	else if (std::holds_alternative<BasicBlock::Branch>(basic_block.jump)) {
		BasicBlock::Branch const& branch = std::get<BasicBlock::Branch>(basic_block.jump);
		return os << " branch (" << branch.condition.value << ") '@" << branch.true_ << " '@" << branch.false_;
	} else if (std::holds_alternative<BasicBlock::Return>(basic_block.jump)) {
		BasicBlock::Return const& return_ = std::get<BasicBlock::Return>(basic_block.jump);
		os << " return ";
		if (return_.value.has_value()) return os << return_.value.value().value;
		else return os << "(void)";
	} else {
		return os << " no jump (invalid)";
	}
}

std::ostream& operator<<(std::ostream& os, Function const& function) {
	os << "declare function @" << function.name.value << " w/ args (";
	size_t count = 0;
	for (auto const& arg : function.arguments) {
		os << '@' << arg.name.value << ": " << arg.type.value;
		if (++count < function.arguments.size()) os << ", ";
	}
	os << ") -> " << function.return_type.value << ": ";
	if (function.body.empty()) return os << "(empty body)";
	os.iword(0)++;
	for (BasicBlock const& basic_block : function.body) {
		os << '\n';
		for (long i = 0; i < os.iword(0); ++i) os << "    ";
		os << basic_block;
	}
	os.iword(0)--;
	return os << '\n';
}

std::ostream& operator<<(std::ostream& os, Struct const& struct_) {
	os << "declare struct @" << struct_.name.value << " w/ fields: {\n";
	os.iword(0)++;
	for (Struct::Field const& field : struct_.fields) {
		for (long i = 0; i < os.iword(0); ++i) os << "    ";
		std::cout << field.name.value << ": " << field.type.value << "\n";
	}
	os.iword(0)--;
	for (long i = 0; i < os.iword(0); ++i) os << "    ";
	return os << "}";
}

}  // namespace IR
