#include "ir.hpp"

#include <iostream>
#include <llvm/ADT/SmallVector.h>

namespace IR {

GenericList clone(GenericList const& generic_list) {
	GenericList new_list {};
	new_list.reserve(generic_list.size());
	std::transform(
		generic_list.cbegin(),
		generic_list.cend(),
		std::back_inserter(new_list),
		[](Spanned<Type> const& type) { return Spanned {type.span, type.value.clone()}; }
	);
	return new_list;
}

std::ostream& operator<<(std::ostream& os, GenericDeclaration const& generic_declaration) {
	os << '<';
	size_t count = 0;
	for (auto const& generic : generic_declaration) {
		os << '@' << generic.name;
		if (!generic.constraints.empty()) {
			size_t subcount = 0;
			os << ": ";
			for (auto const& constraint : generic.constraints) {
				os << '@' << constraint.name << constraint.generic_list;
				if (++subcount < generic.constraints.size()) os << " + ";
			}
		}
		if (++count < generic_declaration.size()) os << ", ";
	}
	return os << '>';
}

std::ostream& operator<<(std::ostream& os, GenericList const& generic_list) {
	if (generic_list.empty()) return os;
	os << '<';
	for (size_t i = 0; i < generic_list.size(); ++i) {
		os << generic_list[i].value;
		if (i + 1 < generic_list.size()) os << ", ";
	}
	return os << '>';
}

bool operator==(GenericList const& a, GenericList const& b) {
	if (a.size() != b.size()) return false;
	for (size_t i = 0; i < a.size(); ++i) {
		if (a.at(i).value != b.at(i).value) return false;
	}
	return true;
}

Type::Atom::Named Type::Atom::Named::clone() const {
	// named types must clone their entire generic list
	GenericList cloned_list {};
	cloned_list.reserve(generic_list.size());
	std::transform(
		generic_list.cbegin(),
		generic_list.cend(),
		std::back_inserter(cloned_list),
		[](Spanned<Type> const& type) { return Spanned {type.span, type.value.clone()}; }
	);

	return {name, std::move(cloned_list)};
}

Type::Atom Type::Atom::clone() const {
	switch (kind()) {
	case Kind::Integer: return make_integer(get_integer().bit_width(), get_integer().is_signed());
	case Kind::Float:   return make_float(get_float().width);
	case Kind::Void:    return make_void();
	case Kind::Char:    return make_char();
	case Kind::Bool:    return make_bool();
	case Kind::Named:   return {get_named().clone()};
	case Kind::Error:   return make_error();
	}
}

Type Type::clone() const {
	if (is_atom()) { return Type::make_atom(get_atom().clone()); }

	Type::Pointer const& pointer = get_pointer();
	return Type::make_pointer(
		Type::Pointer {
			std::make_unique<Spanned<Type>>(Spanned {pointer.type->span, pointer.type->value.clone()}),
			pointer.mutable_
		}
	);
}

std::ostream& operator<<(std::ostream& os, Type::Atom::Named const& named) {
	return os << '@' << named.name.value << named.generic_list;
}

std::ostream& operator<<(std::ostream& os, Type::Atom const& atom) {
	switch (atom.kind()) {
	case Type::Atom::Kind::Integer: break;
	case Type::Atom::Kind::Float:   return os << "float" << (uint32_t) atom.get_float().width_value();
	case Type::Atom::Kind::Void:    return os << "void";
	case Type::Atom::Kind::Char:    return os << "char";
	case Type::Atom::Kind::Bool:    return os << "bool";
	case Type::Atom::Kind::Named:   return os << atom.get_named();
	case Type::Atom::Kind::Error:   return os << "(error)";
	}

	// we know it's an integer now
	Type::Atom::Integer int_ = atom.get_integer();
	os << (int_.is_signed() ? "int" : "uint");
	return os << int_.bit_width();
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

bool operator==(Type::Atom::Integer const& a, Type::Atom::Integer const& b) {
	if (a.is_signed() != b.is_signed()) return false;
	return a.bit_width() == b.bit_width();
}

bool operator==(Type::Atom::Named const& a, Type::Atom::Named const& b) {
	if (a.name.value != b.name.value) return false;
	return a.generic_list == b.generic_list;
}

bool operator==(Type::Atom const& a, Type::Atom const& b) {
	if (a.kind() != b.kind()) return false;
	switch (a.kind()) {
	case Type::Atom::Kind::Integer: return a.get_integer() == b.get_integer();
	case Type::Atom::Kind::Float:   return a.get_float().width == b.get_float().width;
	case Type::Atom::Kind::Void:
	case Type::Atom::Kind::Char:
	case Type::Atom::Kind::Bool:    return true;
	case Type::Atom::Kind::Named:   return a.get_named() == b.get_named();
	case Type::Atom::Kind::Error:   return true;
	}
}

bool operator==(Type::Pointer const& a, Type::Pointer const& b) {
	return a.mutable_ == b.mutable_ && a.type->value == b.type->value;
}

bool operator==(Type const& a, Type const& b) {
	if (a.kind() != b.kind()) return false;
	if (a.is_pointer()) return a.get_pointer() == b.get_pointer();
	if (a.is_atom()) return a.get_atom() == b.get_atom();
	assert(false && "non-exhaustive");
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

Place Place::clone() const {
	switch (kind()) {
	case Kind::Symbol: return make_symbol(get_symbol(), type.clone());
	case Kind::Deref:
		return make_deref(
			std::make_unique<Spanned<Place>>(get_deref().address->span, get_deref().address->value.clone()),
			type.clone()
		);
	case Kind::Access:
		return make_access(
			std::make_unique<Spanned<Place>>(
				get_access().accessee->span,
				get_access().accessee->value.clone()
			),
			get_access().field_index,
			type.clone()
		);
	case Kind::Error: return make_error(type.clone());
	}
}

bool Place::is_prefix_of(Place const& other) const {
	// if we're equal, the condition holds true
	if (*this == other) return true;

	// if the other place is an access, we can "peel back" the access by reducing it to the accessee, i.e. we remove
	// the outermost dot access
	if (other.is_access()) return is_prefix_of(other.get_access().accessee->value);

	// if none of these checks holds, there is simply no way for other to be a prefix of this. this also means that
	// dereferencing is skipped, since it copies!
	return false;
}

Place const& Place::get_access_base() const {
	return is_access() ? get_access().accessee->value.get_access_base() : *this;
}

std::ostream& operator<<(std::ostream& os, Place::Deref const& deref) {
	return os << "(*" << deref.address->value << ')';
}

std::ostream& operator<<(std::ostream& os, Place::Access const& access) {
	return os << '(' << access.accessee->value << ".+" << access.field_index << ')';
}

std::ostream& operator<<(std::ostream& os, Place const& place) {
	switch (place.kind()) {
	case Place::Kind::Symbol: return os << '@' << place.get_symbol();
	case Place::Kind::Deref:  return os << place.get_deref();
	case Place::Kind::Access: return os << place.get_access();
	case Place::Kind::Error:  return os << "(error)";
	}
}

bool operator==(Place const& a, Place const& b) {
	if (a.kind() != b.kind()) return false;
	switch (a.kind()) {
	case Place::Kind::Symbol: return a.get_symbol() == b.get_symbol();
	case Place::Kind::Deref:  return a.get_deref().address->value == b.get_deref().address->value;
	case Place::Kind::Access:
		return a.get_access().field_index == b.get_access().field_index
		    && a.get_access().accessee->value == b.get_access().accessee->value;
	case Place::Kind::Error: return true;
	}
}

Value::Atom Value::Atom::clone() const {
	switch (kind()) {
	case Kind::Identifier:    return make_identifier(get_identifier(), type.clone());
	case Kind::Literal:       return {get_literal(), type.clone()};
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
		StructLiteral {get_struct_literal().type.clone(), std::move(fields)},
		type.clone()
	};
}

std::ostream& operator<<(std::ostream& os, Value::Atom::Literal const& literal) {
	switch (literal.kind) {
	case Value::Atom::Literal::Kind::Int: {
		// FIXME: this is extremely silly
		llvm::SmallVector<char> value;
		std::get<llvm::APSInt>(literal.literal).toString(value);
		std::string actual_value;
		actual_value.reserve(value.size());
		for (size_t i = 0; i < value.size(); ++i) actual_value.push_back(value[i]);
		return os << "(int) " << actual_value;
	}
	case Value::Atom::Literal::Kind::Float: {
		// FIXME: this is extremely silly
		llvm::SmallVector<char> value;
		std::get<llvm::APFloat>(literal.literal).toString(value);
		std::string actual_value;
		actual_value.reserve(value.size());
		for (size_t i = 0; i < value.size(); ++i) actual_value.push_back(value[i]);
		return os << "(float) " << actual_value;
	}
	case Value::Atom::Literal::Kind::Char:
		// FIXME: this breaks for escaped chars
		return os << "(char) '" << std::get<char>(literal.literal) << "'";
	case Value::Atom::Literal::Kind::Bool:
		return os << "(bool) " << (std::get<bool>(literal.literal) ? "true" : "false");
	}
}

std::ostream& operator<<(std::ostream& os, Value::Atom::StructLiteral const& struct_literal) {
	os << "[struct literal of type " << struct_literal.type << " with ";
	if (struct_literal.fields.empty()) return os << "no fields]";
	os << "fields ";
	size_t count = 0;
	for (auto const& field : struct_literal.fields) {
		os << field.value;
		if (++count < struct_literal.fields.size()) os << ", ";
	}
	return os << ']';
}

std::ostream& operator<<(std::ostream& os, Value::Atom const& atom) {
	switch (atom.kind()) {
	case Value::Atom::Kind::Identifier:    return os << '@' << atom.get_identifier();
	case Value::Atom::Kind::Literal:       return os << atom.get_literal();
	case Value::Atom::Kind::StructLiteral: return os << atom.get_struct_literal();
	case Value::Atom::Kind::Error:         return os << "(error)";
	}
	[[assume(false)]];
}

std::ostream& operator<<(std::ostream& os, Value::FunctionCall const& call) {
	os << "(call ";
	if (std::holds_alternative<Spanned<Identifier>>(call.callee))
		os << '@' << std::get<Spanned<Identifier>>(call.callee).value;
	else os << std::get<BuiltInFunction>(call.callee);
	os << call.generic_list;
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

std::ostream& operator<<(std::ostream& os, Value::Ref const& ref) {
	return os << "(&" << (ref.mutable_ ? "mut" : "const") << " (" << ref.value.value << "))";
}

std::ostream& operator<<(std::ostream& os, Value::Load const& load) {
	return os << load.value.value;
}

std::ostream& operator<<(std::ostream& os, Value const& value) {
	switch (value.kind()) {
	case Value::Kind::Atom:         return os << value.get_atom();
	case Value::Kind::FunctionCall: return os << value.get_function_call();
	case Value::Kind::Ref:          return os << value.get_ref();
	case Value::Kind::Load:         return os << value.get_load();
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
	os << set.place.value << " = " << set.value.value;
	return os << ";]";
}

std::ostream& operator<<(std::ostream& os, Statement const& statement) {
	for (long i = 0; i < os.iword(0); ++i) os << "    ";
	switch (statement.kind()) {
	case Statement::Kind::Declare: return os << statement.get_declare();
	case Statement::Kind::Set:     return os << statement.get_set();
	case Statement::Kind::Call:    return os << "[call stmt: " << statement.get_call() << ";]";
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

BasicBlock& Function::find_block(BasicBlock::ID id) {
	return *std::find_if(body.begin(), body.end(), [id](IR::BasicBlock const& block) { return block.id == id; });
}

std::ostream& operator<<(std::ostream& os, Function const& function) {
	os
		<< "declare function @"
		<< function.name.value
		<< " w/ generics "
		<< function.generic_declaration
		<< " w/ args (";
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
	os
		<< "declare struct @"
		<< struct_.name.value
		<< " w/ generics "
		<< struct_.generic_declaration
		<< " w/ fields: {\n";
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
