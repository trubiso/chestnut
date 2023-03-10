use super::types::{ScopeRecursive, Stmt, TokenParser};
use crate::parser::{
	bare_expr_stmt, class_stmt, create_stmt, declare_stmt, func_stmt, let_stmt, return_stmt,
	stmt::assg::assg,
};
use chumsky::prelude::*;

mod assg;

/// Parses any statement, with or without a semicolon, into Stmt
pub fn stmt(scope: ScopeRecursive, semi: bool) -> impl TokenParser<Stmt> + '_ {
	let s = if semi { 1 } else { 0 };
	macro_rules! semi {
		(Y $thing:expr) => {
			$thing.then_ignore(jpunct!(Semicolon).repeated().at_least(s))
		};
		(N $thing:expr) => {
			$thing.then_ignore(jpunct!(Semicolon).repeated())
		};
	}
	choice((
		semi!(Y let_stmt()),
		// TODO: mut_stmt (`mut <ident> = <expr>`)
		semi!(Y create_stmt()),
		semi!(Y declare_stmt()),
		semi!(N func_stmt(scope.clone())),
		semi!(N class_stmt(scope)),
		semi!(Y return_stmt()),
		semi!(Y assg()),
		semi!(Y bare_expr_stmt()),
	))
}
