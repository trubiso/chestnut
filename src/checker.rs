//! Checks whether statements and their privacy are correct according to the
//! context, as well as the casing of variable and type names.

use self::case::{check_case_ident, Case};
use crate::{
	common::{Privacy, Stmt},
	parser::types::{ParserScope, ParserStmt},
	span::IntoSpan,
};
use codespan_reporting::diagnostic::{Diagnostic, Label};
use derive_more::Display;
use lazy_static::lazy_static;
use std::sync::Mutex;

pub mod case;

lazy_static! {
	static ref DIAGNOSTICS: Mutex<Vec<Diagnostic<usize>>> = Mutex::new(vec![]);
}

pub fn add_diagnostic(diagnostic: Diagnostic<usize>) {
	DIAGNOSTICS.lock().unwrap().push(diagnostic);
}

#[derive(Debug, Display, Clone, PartialEq, Eq)]
pub enum Context {
	#[display(fmt = "top level")]
	TopLevel,
	#[display(fmt = "class")]
	Class,
	#[display(fmt = "function")]
	Func,
	#[display(fmt = "unsafe")]
	Unsafe,
	#[display(fmt = "limited unsafe")]
	LimitedUnsafe,
}

macro_rules! check_stmt {
	($($v:ident => $($ctx:ident)*;)*) => {
		fn check_stmt(stmt: &ParserStmt, context: &Context) {
			match stmt {
				$(
					ParserStmt::$v(..) => {
						if $(*context != Context::$ctx)&&* {
							add_diagnostic(
								Diagnostic::error()
									.with_message(format!("invalid {} statement in {context} context", stmt.variant()))
									.with_labels(vec![Label::primary(stmt.span().file_id, stmt.span().range())]),
							);
						}
					}
				)*
			}
		}
	};
}

check_stmt!(
	Create => TopLevel Class Func Unsafe;
	Declare => TopLevel Class Func Unsafe;
	Set => TopLevel Func Unsafe;
	Func => TopLevel Class Func Unsafe; // NOTE: maybe not Func
	Return => Func;
	Class => TopLevel Func; // NOTE: maybe Class too?
	Import => TopLevel Unsafe;
	BareExpr => Func Unsafe;
	Unsafe => Func TopLevel Class; // TopLevel and Class turn it into LimitedUnsafe
	Cpp => Unsafe LimitedUnsafe;
);

fn check_privacy(privacy: &Privacy, context: &Context) {
	let is_valid = privacy.is_default()
		|| match context {
			Context::TopLevel => privacy.is_export(),
			Context::Class => !privacy.is_export(),
			Context::Func => false,
			Context::Unsafe | Context::LimitedUnsafe => false,
		};
	if !is_valid {
		let span = privacy.span(); // never Default => never None
		add_diagnostic(
			Diagnostic::error()
				.with_message(format!(
					"invalid privacy qualifier {privacy}in {context} context"
				))
				.with_labels(vec![Label::primary(span.file_id, span.range())]),
		);
	}
}

fn check_inner(scope: &ParserScope, context: Context) {
	for stmt in &scope.stmts {
		check_stmt(stmt, &context);
		match stmt {
			Stmt::Create(_, x, ty_ident, ..) | Stmt::Declare(_, x, ty_ident, ..) => {
				check_case_ident(&ty_ident.ident, Case::SnakeCase);
				check_privacy(x, &context);
			}
			Stmt::Func(_, x, i, f) => {
				check_case_ident(i, Case::SnakeCase);
				check_privacy(x, &context);
				check_inner(&f.body, Context::Func);
			}
			Stmt::Class(_, x, i, g, _, b) => {
				check_case_ident(i, Case::PascalCase);
				for x in g.iter() {
					check_case_ident(x, Case::PascalCase);
				}
				check_privacy(x, &context);
				check_inner(b, Context::Class);
			}
			Stmt::Unsafe(_, b) => {
				check_inner(
					b,
					if matches!(context, Context::TopLevel | Context::Class) {
						Context::LimitedUnsafe
					} else {
						Context::Unsafe
					},
				);
			}
			_ => {}
		}
	}
}

pub fn check(scope: &ParserScope) -> Vec<Diagnostic<usize>> {
	check_inner(scope, Context::TopLevel);
	if !DIAGNOSTICS.lock().unwrap().is_empty() {
		let diagnostics = DIAGNOSTICS.lock().unwrap();
		diagnostics.clone()
	} else {
		vec![]
	}
}
