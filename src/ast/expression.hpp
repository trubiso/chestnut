#pragma once
#include "../token.hpp"
#include "generics.hpp"
#include "identifier.hpp"

#include <memory>
#include <optional>
#include <variant>
#include <vector>

namespace AST {

struct Expression {
	struct Atom {
		enum class Kind {
			Identifier,
			NumberLiteral,
			StringLiteral,
			CharLiteral,
			BoolLiteral,
			StructLiteral,
			Expression
		};

		struct NumberLiteral {
			std::string               literal;
			std::optional<Identifier> suffix;  // unqualified

			bool is_float() const;
		};

		struct StringLiteral {
			std::string               literal;
			std::optional<Identifier> suffix;  // unqualified
		};

		struct CharLiteral {
			std::string               literal;
			std::optional<Identifier> suffix;  // unqualified
		};

		struct BoolLiteral {
			bool value;
		};

		struct StructLiteral {
			Spanned<Identifier> name;

			struct Field {
				Spanned<std::string>                 name;
				std::unique_ptr<Spanned<Expression>> value;
			};

			std::vector<Field> fields;
			/// Whether the struct literal has the correct amount of fields.
			bool valid = true;
		};

		typedef std::variant<
			Identifier,
			NumberLiteral,
			StringLiteral,
			CharLiteral,
			BoolLiteral,
			StructLiteral,
			std::unique_ptr<Expression>>
			value_t;

		value_t value;

		inline constexpr Kind kind() const { return (Kind) value.index(); }

		inline static Atom make_identifier(Identifier&& identifier) {
			return Atom(value_t {std::in_place_index<(size_t) Kind::Identifier>, identifier});
		}

		inline static Atom make_number_literal(std::string literal, std::optional<Identifier> suffix) {
			return Atom(
				value_t {
					std::in_place_index<(size_t) Kind::NumberLiteral>,
					NumberLiteral {literal, suffix}
                        }
			);
		}

		inline static Atom make_string_literal(std::string literal, std::optional<Identifier> suffix) {
			return Atom(
				value_t {
					std::in_place_index<(size_t) Kind::StringLiteral>,
					StringLiteral {literal, suffix}
                        }
			);
		}

		inline static Atom make_char_literal(std::string literal, std::optional<Identifier> suffix) {
			return Atom(
				value_t {
					std::in_place_index<(size_t) Kind::CharLiteral>,
					CharLiteral {literal, suffix}
                        }
			);
		}

		inline static Atom make_bool_literal(bool value) {
			return Atom(value_t {std::in_place_index<(size_t) Kind::BoolLiteral>, BoolLiteral {value}});
		}

		inline static Atom
		make_struct_literal(Spanned<Identifier>&& name, std::vector<StructLiteral::Field>&& fields) {
			return Atom(
				value_t {
					std::in_place_index<(size_t) Kind::StructLiteral>,
					StructLiteral {std::move(name), std::move(fields)}
                        }
			);
		}

		inline static Atom make_expression(std::unique_ptr<Expression>&& expression) {
			return Atom(value_t {std::in_place_index<(size_t) Kind::Expression>, std::move(expression)});
		}

		inline bool is_identifier() const { return kind() == Kind::Identifier; }

		inline bool is_number_literal() const { return kind() == Kind::NumberLiteral; }

		inline bool is_string_literal() const { return kind() == Kind::StringLiteral; }

		inline bool is_char_literal() const { return kind() == Kind::CharLiteral; }

		inline bool is_bool_literal() const { return kind() == Kind::BoolLiteral; }

		inline bool is_struct_literal() const { return kind() == Kind::StructLiteral; }

		inline bool is_expression() const { return kind() == Kind::Expression; }

		inline Identifier const& get_identifier() const { return std::get<(size_t) Kind::Identifier>(value); }

		inline Identifier& get_identifier() { return std::get<(size_t) Kind::Identifier>(value); }

		inline NumberLiteral const& get_number_literal() const {
			return std::get<(size_t) Kind::NumberLiteral>(value);
		}

		inline StringLiteral const& get_string_literal() const {
			return std::get<(size_t) Kind::StringLiteral>(value);
		}

		inline CharLiteral const& get_char_literal() const {
			return std::get<(size_t) Kind::CharLiteral>(value);
		}

		inline BoolLiteral get_bool_literal() const { return std::get<(size_t) Kind::BoolLiteral>(value); }

		inline StructLiteral const& get_struct_literal() const {
			return std::get<(size_t) Kind::StructLiteral>(value);
		}

		inline StructLiteral& get_struct_literal() { return std::get<(size_t) Kind::StructLiteral>(value); }

		inline std::unique_ptr<Expression> const& get_expression() const {
			return std::get<(size_t) Kind::Expression>(value);
		}

		inline std::unique_ptr<Expression>& get_expression() {
			return std::get<(size_t) Kind::Expression>(value);
		}
	};

	// TODO: change these token symbol operations to be a restricted subset maybe

	struct UnaryOperation {
		std::unique_ptr<Spanned<Expression>> operand;
		Token::Symbol                        operation;
	};

	struct AddressOperation {
		std::unique_ptr<Spanned<Expression>> operand;
		bool                                 mutable_;
	};

	struct BinaryOperation {
		std::unique_ptr<Spanned<Expression>> lhs;
		std::unique_ptr<Spanned<Expression>> rhs;
		Token::Symbol                        operation;
	};

	struct FunctionCall {
		std::unique_ptr<Spanned<Expression>> callee;

		typedef Spanned<Expression>                              OrderedArgument;
		typedef std::tuple<Spanned<Identifier>, OrderedArgument> LabeledArgument;

		typedef std::variant<OrderedArgument, LabeledArgument> Argument;

		struct Arguments {
			std::vector<OrderedArgument> ordered;
			std::vector<LabeledArgument> labeled;
		} arguments;

		std::optional<GenericList> generic_list;
	};

	struct If {
		std::unique_ptr<Spanned<Expression>> condition;
		std::unique_ptr<Spanned<Expression>> true_;
		std::unique_ptr<Spanned<Expression>> false_;
	};

	struct MemberAccess {
		std::unique_ptr<Spanned<Expression>> accessee;
		Spanned<std::string>                 field;
	};

	enum class Kind { Atom, UnaryOperation, AddressOperation, BinaryOperation, FunctionCall, If, MemberAccess };

	typedef std::variant<Atom, UnaryOperation, AddressOperation, BinaryOperation, FunctionCall, If, MemberAccess>
		value_t;

	value_t value;
	// TODO: move TypeInfo::ID elsewhere to have it here
	std::optional<uint32_t> type = {};

	inline constexpr Kind kind() const { return (Kind) value.index(); }

	inline static Expression make_atom(Atom&& atom) {
		return Expression(value_t {std::in_place_index<(size_t) Kind::Atom>, std::move(atom)});
	}

	inline static Expression
	make_unary_operation(std::unique_ptr<Spanned<Expression>>&& operand, Token::Symbol operation) {
		return Expression(
			value_t {
				std::in_place_index<(size_t) Kind::UnaryOperation>,
				UnaryOperation {std::move(operand), operation}
                }
		);
	}

	inline static Expression make_address_operation(std::unique_ptr<Spanned<Expression>>&& operand, bool mutable_) {
		return Expression(
			value_t {
				std::in_place_index<(size_t) Kind::AddressOperation>,
				AddressOperation {std::move(operand), mutable_}
                }
		);
	}

	inline static Expression make_binary_operation(
		std::unique_ptr<Spanned<Expression>>&& lhs,
		std::unique_ptr<Spanned<Expression>>&& rhs,
		Token::Symbol                          operation
	) {
		return Expression(
			value_t {
				std::in_place_index<(size_t) Kind::BinaryOperation>,
				BinaryOperation {std::move(lhs), std::move(rhs), operation}
                }
		);
	}

	inline static Expression make_function_call(
		std::unique_ptr<Spanned<Expression>>&& callee,
		Expression::FunctionCall::Arguments&&  arguments,
		std::optional<GenericList>             generic_list = std::nullopt
	) {
		return Expression(
			value_t {
				std::in_place_index<(size_t) Kind::FunctionCall>,
				FunctionCall {std::move(callee), std::move(arguments), std::move(generic_list)}
                }
		);
	}

	inline static Expression
	make_if(std::unique_ptr<Spanned<Expression>>&& condition,
	        std::unique_ptr<Spanned<Expression>>&& true_,
	        std::unique_ptr<Spanned<Expression>>&& false_) {
		return Expression(
			value_t {
				std::in_place_index<(size_t) Kind::If>,
				If {std::move(condition), std::move(true_), std::move(false_)}
                }
		);
	}

	inline static Expression
	make_member_access(std::unique_ptr<Spanned<Expression>>&& accessee, Spanned<std::string>&& field) {
		return Expression(
			value_t {
				std::in_place_index<(size_t) Kind::MemberAccess>,
				MemberAccess {std::move(accessee), std::move(field)}
                }
		);
	}

	inline bool is_atom() const { return kind() == Kind::Atom; }

	inline bool is_unary_operation() const { return kind() == Kind::UnaryOperation; }

	inline bool is_address_operation() const { return kind() == Kind::AddressOperation; }

	inline bool is_binary_operation() const { return kind() == Kind::BinaryOperation; }

	inline bool is_function_call() const { return kind() == Kind::FunctionCall; }

	inline bool is_if() const { return kind() == Kind::If; }

	inline bool is_member_access() const { return kind() == Kind::MemberAccess; }

	inline Atom const& get_atom() const { return std::get<(size_t) Kind::Atom>(value); }

	inline Atom& get_atom() { return std::get<(size_t) Kind::Atom>(value); }

	inline UnaryOperation const& get_unary_operation() const {
		return std::get<(size_t) Kind::UnaryOperation>(value);
	}

	inline UnaryOperation& get_unary_operation() { return std::get<(size_t) Kind::UnaryOperation>(value); }

	inline AddressOperation const& get_address_operation() const {
		return std::get<(size_t) Kind::AddressOperation>(value);
	}

	inline AddressOperation& get_address_operation() { return std::get<(size_t) Kind::AddressOperation>(value); }

	inline BinaryOperation const& get_binary_operation() const {
		return std::get<(size_t) Kind::BinaryOperation>(value);
	}

	inline BinaryOperation& get_binary_operation() { return std::get<(size_t) Kind::BinaryOperation>(value); }

	inline FunctionCall const& get_function_call() const { return std::get<(size_t) Kind::FunctionCall>(value); }

	inline FunctionCall& get_function_call() { return std::get<(size_t) Kind::FunctionCall>(value); }

	inline If const& get_if() const { return std::get<(size_t) Kind::If>(value); }

	inline If& get_if() { return std::get<(size_t) Kind::If>(value); }

	inline MemberAccess const& get_member_access() const { return std::get<(size_t) Kind::MemberAccess>(value); }

	inline MemberAccess& get_member_access() { return std::get<(size_t) Kind::MemberAccess>(value); }

	bool can_be_lhs() const;
};

std::ostream& operator<<(std::ostream&, Expression::Atom::NumberLiteral const&);
std::ostream& operator<<(std::ostream&, Expression::Atom::StringLiteral const&);
std::ostream& operator<<(std::ostream&, Expression::Atom::CharLiteral const&);
std::ostream& operator<<(std::ostream&, Expression::Atom::StructLiteral const&);
std::ostream& operator<<(std::ostream&, Expression::Atom const&);
std::ostream& operator<<(std::ostream&, Expression::UnaryOperation const&);
std::ostream& operator<<(std::ostream&, Expression::AddressOperation const&);
std::ostream& operator<<(std::ostream&, Expression::BinaryOperation const&);
std::ostream& operator<<(std::ostream&, Expression::FunctionCall const&);
std::ostream& operator<<(std::ostream&, Expression::If const&);
std::ostream& operator<<(std::ostream&, Expression::MemberAccess const&);
std::ostream& operator<<(std::ostream&, Expression const&);

}  // namespace AST
