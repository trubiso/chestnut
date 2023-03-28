use super::types::{ParserStmt, ScopeRecursive};
use crate::parser::{
	bare_expr_stmt, class_stmt, cpp_stmt, create_stmt, declare_stmt, func_stmt, import_stmt,
	let_stmt, mut_stmt, return_stmt, stmt::assg::assg, unsafe_scope_stmt,
};
use chumsky::prelude::*;

mod assg;

/// Parses any statement, with or without a semicolon, into ParserStmt
pub fn stmt(scope: ScopeRecursive, semi: bool) -> token_parser!(ParserStmt : '_) {
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
		semi!(Y import_stmt()),
		semi!(Y return_stmt()),
		semi!(Y create_stmt()),
		semi!(Y mut_stmt()),
		semi!(Y declare_stmt()),
		semi!(N func_stmt(scope.clone())),
		semi!(N class_stmt(scope.clone())),
		semi!(Y assg()),
		semi!(Y bare_expr_stmt()),
		semi!(N unsafe_scope_stmt(scope)),
		semi!(Y cpp_stmt()),
	))
}
