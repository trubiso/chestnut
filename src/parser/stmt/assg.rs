use crate::common::UnscopedExpr;
use crate::lexer::Operator;
use crate::parser::expr::expr;
use crate::parser::types::{ParserExpr, ParserStmt};
use chumsky::prelude::*;

macro_rules! assg_stmt {
	($ident:ident) => {
		assg!($ident).map_with_span(|((lhs, _), rhs), span| ParserStmt::Set(span, lhs, rhs))
	};
	($ident:ident => $op:ident) => {
		assg!($ident).map_with_span(|((lhs, _), rhs), span| {
			ParserStmt::Set(
				span.clone(),
				lhs.clone(),
				ParserExpr::Unscoped(UnscopedExpr::BinaryOp(
					span,
					Box::new(UnscopedExpr::Identifier(lhs.clone().span(), lhs)),
					Operator::$op,
					Box::new(rhs),
				)),
			)
		})
	};
}

/// Parses `<ident> [-|*|+|/]= <expr>` into ParserStmt::Set
pub fn assg() -> token_parser!(ParserStmt) {
	choice((
		assg_stmt!(Set),
		assg_stmt!(NegSet => Neg),
		assg_stmt!(StarSet => Star),
		assg_stmt!(PlusSet => Plus),
		assg_stmt!(DivSet => Div),
	))
}
