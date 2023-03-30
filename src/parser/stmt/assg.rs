use crate::parser::expr::expr;
use crate::parser::types::{ParserExpr, ParserStmt};
use crate::span::IntoSpan;
use chumsky::prelude::*;

macro_rules! assg_stmt {
	($ident:ident) => {
		assg!($ident).map_with_span(|((lhs, _), rhs), span| ParserStmt::Set(span, lhs, rhs))
	};
	(operator $op:ident) => {
		assg!($op -> Set).map_with_span(|(((lhs, op), _), rhs), span| {
			ParserStmt::Set(
				span.clone(),
				lhs.clone(),
				ParserExpr::BinaryOp(
					span,
					Box::new(ParserExpr::Identifier(lhs.clone().span(), lhs)),
					force_token!(op => Operator),
					Box::new(rhs),
				),
			)
		})
	};
}

/// Parses `<ident> [-|*|+|/]= <expr>` into ParserStmt::Set
pub fn assg() -> token_parser!(ParserStmt) {
	choice((
		assg_stmt!(Set),
		assg_stmt!(operator Neg),
		assg_stmt!(operator Star),
		assg_stmt!(operator Plus),
		assg_stmt!(operator Div),
		assg_stmt!(operator And),
		assg_stmt!(operator Or),
	))
}
