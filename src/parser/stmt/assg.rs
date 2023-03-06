use crate::lexer::Operator;
use crate::parser::expr::expr;
use crate::parser::types::{Expr, Stmt, TokenParser};
use chumsky::prelude::*;

macro_rules! assg_stmt {
	($ident:ident) => {
		assg!($ident).map_with_span(|((lhs, _), rhs), span| Stmt::Set(span, lhs, rhs))
	};
	($ident:ident => $op:ident) => {
		assg!($ident).map_with_span(|((lhs, _), rhs), span| {
			Stmt::Set(
				span.clone(),
				lhs.clone(),
				Expr::BinaryOp(
					span,
					Box::new(Expr::Identifier(lhs.clone().span(), lhs)),
					Operator::$op,
					Box::new(rhs),
				),
			)
		})
	};
}

pub fn assg() -> impl TokenParser<Stmt> {
	choice((
		assg_stmt!(Set),
		assg_stmt!(NegSet => Neg),
		assg_stmt!(StarSet => Star),
		assg_stmt!(PlusSet => Plus),
		assg_stmt!(DivSet => Div),
	))
}
