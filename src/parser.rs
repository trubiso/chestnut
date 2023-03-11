use crate::{
	lexer::{Keyword, Token},
	span::{Span, Spanned},
};
use chumsky::{error::SimpleReason, prelude::*, Stream};
use codespan_reporting::diagnostic::{Diagnostic, Label};
use expr::expr;
use ident::*;
use privacy::privacy_attribs;
use std::vec::IntoIter;
use stmt::stmt;
use ty_ident::*;
use types::*;

pub mod types;
#[macro_use]
pub mod macros;
mod expr;
mod ident;
mod privacy;
mod stmt;
mod ty;
mod ty_ident;

macro_rules! func_attribs {
	($($kw:ident => $prop:ident)*) => {
		choice(($(jkeyword!($kw),)*))
		.repeated()
		.validate(|attribs, span: Span, emit| {
			let mut final_attribs = FuncAttribs::default();
			for i in 0..attribs.len() {
				match force_token!(attribs[i] => Keyword) {
					$(
						Keyword::$kw => {
							if final_attribs.$prop {
								emit(chumsky::error::Simple::custom(span.clone(), "cannot apply attribute twice"));
							}
							final_attribs.$prop = true;
						}
					)*
					_ => unreachable!(),
				}
			}
			final_attribs
		})
	};
}

fn func_attribs() -> token_parser!(FuncAttribs) {
	func_attribs!(
		Pure => is_pure
		Mut => is_mut
		Unsafe => is_unsafe
	)
}

/// Parses `<ty ident>, ...` into Vec<TypedIdent>
fn func_args() -> token_parser!(Vec<TypedIdent>) {
	parened!(ty_ident(None),)
}

/// Parses `<<ident>, ...>` into Vec<Ident>
fn generics_declare() -> token_parser!(Vec<Ident>) {
	angled!(ident_nodiscard(),)
		.or_not()
		.map(|x| x.unwrap_or_default())
}

/// Parses `<ty ident>(<ty ident>, ...) { <scope> }` into Stmt::Func
fn func_stmt(scope: ScopeRecursive) -> token_parser!(Stmt : '_) {
	privacy_attribs()
		.then(ty_ident_nodiscard(None))
		.then(generics_declare())
		.then(func_args())
		.then(func_attribs())
		.then(choice((
			jkeyword!(FatArrow)
				.ignore_then(expr())
				.then_ignore(jpunct!(Semicolon))
				.map_with_span(|expr, span| Scope {
					span: span.clone(),
					stmts: vec![Stmt::Return(span, expr)],
				}),
			braced!(scope),
		)))
		.map_with_span(
			|(((((privacy, ty_ident), generics), args), attribs), body), span| {
				Stmt::Func(
					span.clone(),
					privacy,
					ty_ident.ident,
					Func {
						span,
						return_ty: ty_ident.ty,
						args,
						generics,
						body,
						attribs,
					},
				)
			},
		)
}

/// Parses `let <ident> = <expr>;` into Stmt::Create
fn let_stmt() -> token_parser!(Stmt) {
	privacy_attribs()
		.then_ignore(jkeyword!(Let))
		.then(assg!(ignore Set))
		.map_with_span(|(privacy, (lhs, value)), span| {
			Stmt::Create(span, privacy, lhs.infer_type(), value)
		})
}

/// Parses `<ty ident> = <expr>;` into Stmt::Create
fn create_stmt() -> token_parser!(Stmt) {
	privacy_attribs()
		.then(ty_ident(None))
		.then(assg!(noident ignore Set))
		.map_with_span(|((privacy, lhs), value), span| Stmt::Create(span, privacy, lhs, value))
}

/// Parses `<ty ident>;` into Stmt::Declare
fn declare_stmt() -> token_parser!(Stmt) {
	privacy_attribs()
		.then(ty_ident_nodiscard(None))
		.map_with_span(|(privacy, ty_ident), span| Stmt::Declare(span, privacy, ty_ident))
}

/// Parses `<expr>;` into Stmt::BareExpr
///
/// Useful, for example, for function calls where the return value is discarded
fn bare_expr_stmt() -> token_parser!(Stmt) {
	expr().map_with_span(|x, span| Stmt::BareExpr(span, x))
}

fn return_stmt() -> token_parser!(Stmt) {
	jkeyword!(Return)
		.ignore_then(expr())
		.map_with_span(|x, span| Stmt::Return(span, x))
}

fn class_stmt(scope: ScopeRecursive) -> token_parser!(Stmt : '_) {
	privacy_attribs()
		.then_ignore(jkeyword!(Class))
		.then(ident_nodiscard())
		.then(generics_declare())
		.then(braced!(scope))
		.map_with_span(|(((privacy, ident), generics), body), span| {
			Stmt::Class(span, privacy, ident, generics, body)
		})
}

/// Parses `mut <ident> = <expr>;` into Stmt::Create
fn mut_stmt() -> token_parser!(Stmt) {
	privacy_attribs()
		.then_ignore(jkeyword!(Mut))
		.then(assg!(ignore Set))
		.map_with_span(|(privacy, (lhs, value)), span| {
			Stmt::Create(span, privacy, lhs.infer_type().make_mut(), value)
		})
}

/// Parses `import <qualified_ident>::[{ident_nodiscard, ...}|*]` into Stmt::Import
fn import_stmt() -> token_parser!(Stmt) {
	jkeyword!(Import)
		.ignore_then(qualified_ident())
		.then(jpunct!(ColonColon).then(jop!(Star)).map(|_| true).or_not())
		.map_with_span(|(imported, glob), span| Stmt::Import(span, glob.is_some(), imported))
}

/// Parses `unsafe <scope>` into Stmt::Unsafe
fn unsafe_scope_stmt(scope: ScopeRecursive) -> token_parser!(Stmt : '_) {
	jkeyword!(Unsafe)
		.ignore_then(braced!(scope))
		.map_with_span(|scope, span| Stmt::Unsafe(span, scope))
}

/// Parses a bare scope (not wrapped in curly braces) into Scope
pub fn bare_scope() -> token_parser!(Scope) {
	recursive(|scope| {
		stmt(scope, true)
			.repeated()
			.map_with_span(|x, span| Scope { span, stmts: x })
	})
}

pub fn parser() -> token_parser!(Scope) {
	bare_scope().then_ignore(end())
}

pub type CodeStream<'a> = Stream<'a, Token, Span, IntoIter<Spanned<Token>>>;

pub fn parse(code_stream: CodeStream) -> Result<Scope, Vec<Diagnostic<usize>>> {
	let (parsed, errors) = parser().parse_recovery(code_stream);
	let mut diagnostics = vec![];
	if errors.is_empty() {
		return Ok(parsed.expect("what"));
	}
	// try not to duplicate diagnostics challenge
	let mut add_diagnostic = |diagnostic: Diagnostic<_>| {
		if !diagnostics.contains(&diagnostic) {
			diagnostics.push(diagnostic);
		}
	};
	for err in errors {
		match err.reason() {
			SimpleReason::Unclosed { span, delimiter } => add_diagnostic(
				Diagnostic::error()
					.with_message(format!("unclosed delimiter {delimiter}"))
					.with_labels(vec![
						Label::primary(err.span().file_id, err.span().range())
							.with_message("invalid delimiter"),
						Label::secondary(span.file_id, span.range())
							.with_message("opening delimiter here"),
					]),
			),
			SimpleReason::Unexpected => add_diagnostic(
				Diagnostic::error()
					.with_message("unexpected token")
					.with_labels(vec![Label::primary(err.span().file_id, err.span().range())
						.with_message("this token is invalid")])
					.with_notes(vec![format!(
						"expected one of {}",
						err.expected()
							.map(|x| format!("'{}'", x.as_ref().unwrap_or(&Token::Error)))
							.reduce(|acc, b| acc + ", " + &b)
							.unwrap_or("".into())
					)]),
			),
			SimpleReason::Custom(label) => add_diagnostic(
				Diagnostic::error()
					.with_message(label)
					.with_labels(vec![Label::primary(err.span().file_id, err.span().range())]),
			),
		}
	}
	Err(diagnostics)
}
