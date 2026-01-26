#pragma once
#include "../span.hpp"
#include "expression.hpp"
#include "identifier.hpp"
#include "type.hpp"

#include <optional>
#include <variant>
#include <vector>

namespace AST {

struct Statement;

typedef std::vector<Spanned<Statement>> Scope;

struct Statement {
	struct Declare {
		Spanned<Identifier>                name;  // unqualified
		std::optional<Spanned<Type>>       type;
		std::optional<Spanned<Expression>> value;

		Spanned<bool> mutable_;
	};

	struct Set {
		Spanned<Expression> lhs;
		Spanned<Expression> rhs;
	};

	struct Return {
		std::optional<Spanned<Expression>> value;
	};

	enum class Kind {
		Declare    = 0,
		Set        = 1,
		Expression = 2,
		Return     = 3,
		Scope      = 4,
	};

	typedef std::variant<Declare, Set, Expression, Return, Scope> value_t;

	value_t value;

	inline constexpr Kind kind() const { return (Kind) value.index(); }

	inline static Statement make_declare(Declare&& declare) {
		return Statement(value_t {std::in_place_index<(size_t) Kind::Declare>, std::move(declare)});
	}

	inline static Statement make_set(Set&& set) {
		return Statement(value_t {std::in_place_index<(size_t) Kind::Set>, std::move(set)});
	}

	inline static Statement make_expression(Expression&& expression) {
		return Statement(value_t {std::in_place_index<(size_t) Kind::Expression>, std::move(expression)});
	}

	inline static Statement make_return(Return&& return_) {
		return Statement(value_t {std::in_place_index<(size_t) Kind::Return>, std::move(return_)});
	}

	inline static Statement make_scope(Scope&& scope) {
		return Statement(value_t {std::in_place_index<(size_t) Kind::Scope>, std::move(scope)});
	}

	inline Declare const& get_declare() const { return std::get<(size_t) Kind::Declare>(value); }

	inline Declare& get_declare() { return std::get<(size_t) Kind::Declare>(value); }

	inline Set const& get_set() const { return std::get<(size_t) Kind::Set>(value); }

	inline Set& get_set() { return std::get<(size_t) Kind::Set>(value); }

	inline Expression const& get_expression() const { return std::get<(size_t) Kind::Expression>(value); }

	inline Expression& get_expression() { return std::get<(size_t) Kind::Expression>(value); }

	inline Return const& get_return() const { return std::get<(size_t) Kind::Return>(value); }

	inline Return& get_return() { return std::get<(size_t) Kind::Return>(value); }

	inline Scope const& get_scope() const { return std::get<(size_t) Kind::Scope>(value); }

	inline Scope& get_scope() { return std::get<(size_t) Kind::Scope>(value); }
};

std::ostream& operator<<(std::ostream&, Statement::Declare const&);
std::ostream& operator<<(std::ostream&, Statement::Set const&);
std::ostream& operator<<(std::ostream&, Statement::Return const&);
std::ostream& operator<<(std::ostream&, Statement const&);

}  // namespace AST
