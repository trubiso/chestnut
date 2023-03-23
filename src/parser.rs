use crate::{
	common::{FuncAttribs, TypedIdent},
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

/// Parses `<ty ident>(<ty ident>, ...) { <scope> }` into ParserStmt::Func
fn func_stmt(scope: ScopeRecursive) -> token_parser!(ParserStmt : '_) {
	privacy_attribs()
		.then(ty_ident_nodiscard(None))
		.then(generics_declare())
		.then(func_args())
		.then(func_attribs())
		.map_with_span(|pre, span| (pre, span))
		.then(choice((
			jkeyword!(FatArrow)
				.ignore_then(expr())
				.then_ignore(jpunct!(Semicolon))
				.map_with_span(|expr, span| ParserScope {
					span: span.clone(),
					stmts: vec![ParserStmt::Return(span, expr)],
				}),
			braced!(scope),
		)))
		.map_with_span(
			|((((((privacy, ty_ident), generics), args), attribs), decl_span), body), span| {
				ParserStmt::Func(
					span.clone(),
					privacy,
					ty_ident.ident,
					ParserFunc {
						span,
						return_ty: ty_ident.ty,
						args,
						generics,
						body,
						attribs,
						decl_span,
						_expr: std::marker::PhantomData,
					},
				)
			},
		)
}

/// Parses `let <ident> = <expr>;` into ParserStmt::Create
fn let_stmt() -> token_parser!(ParserStmt) {
	privacy_attribs()
		.then_ignore(jkeyword!(Let))
		.then(assg!(ignore Set))
		.map_with_span(|(privacy, (lhs, value)), span| {
			ParserStmt::Create(span, privacy, lhs.infer_type(), false, value)
		})
}

/// Parses `<ty ident> = <expr>;` into ParserStmt::Create
fn create_stmt() -> token_parser!(ParserStmt) {
	privacy_attribs()
		.then(jkeyword!(Mut).map(|_| true).or_not())
		.then(ty_ident(None))
		.then(assg!(noident ignore Set))
		.map_with_span(|(((privacy, mutness), lhs), value), span| {
			ParserStmt::Create(span, privacy, lhs, mutness.is_some(), value)
		})
}

/// Parses `<ty ident>;` into ParserStmt::Declare
fn declare_stmt() -> token_parser!(ParserStmt) {
	privacy_attribs()
		.then(jkeyword!(Mut).map(|_| true).or_not())
		.then(ty_ident_nodiscard(None))
		.map_with_span(|((privacy, mutness), ty_ident), span| {
			ParserStmt::Declare(span, privacy, ty_ident, mutness.is_some())
		})
}

/// Parses `<expr>;` into ParserStmt::BareExpr
///
/// Useful, for example, for function calls where the return value is discarded
fn bare_expr_stmt() -> token_parser!(ParserStmt) {
	expr().map_with_span(|x, span| ParserStmt::BareExpr(span, x))
}

fn return_stmt() -> token_parser!(ParserStmt) {
	jkeyword!(Return)
		.ignore_then(expr())
		.map_with_span(|x, span| ParserStmt::Return(span, x))
}

fn class_stmt(scope: ScopeRecursive) -> token_parser!(ParserStmt : '_) {
	privacy_attribs()
		.then_ignore(jkeyword!(Class))
		.then(ident_nodiscard())
		.then(generics_declare())
		.map_with_span(|pre, span| (pre, span))
		.then(braced!(scope))
		.map_with_span(|((((privacy, ident), generics), decl_span), body), span| {
			ParserStmt::Class(span, privacy, ident, generics, decl_span, body)
		})
}

/// Parses `mut <ident> = <expr>;` into ParserStmt::Create
fn mut_stmt() -> token_parser!(ParserStmt) {
	privacy_attribs()
		.then_ignore(jkeyword!(Mut))
		.then(assg!(ignore Set))
		.map_with_span(|(privacy, (lhs, value)), span| {
			ParserStmt::Create(span, privacy, lhs.infer_type(), true, value)
		})
}

/// Parses `import <qualified_ident>::[{ident_nodiscard, ...}|*]` into
/// ParserStmt::Import
fn import_stmt() -> token_parser!(ParserStmt) {
	jkeyword!(Import)
		.ignore_then(qualified_ident())
		.then(jpunct!(ColonColon).then(jop!(Star)).map(|_| true).or_not())
		.map_with_span(|(imported, glob), span| ParserStmt::Import(span, glob.is_some(), imported))
}

/// Parses `unsafe <scope>` into ParserStmt::Unsafe
fn unsafe_scope_stmt(scope: ScopeRecursive) -> token_parser!(ParserStmt : '_) {
	jkeyword!(Unsafe)
		.ignore_then(braced!(scope))
		.map_with_span(|scope, span| ParserStmt::Unsafe(span, scope))
}

/// Parses `cpp <string literal>` into ParserStmt::Cpp
fn cpp_stmt() -> token_parser!(ParserStmt) {
	jkeyword!(Cpp)
		.ignore_then(filter(|x| matches!(x, Token::StringLiteral(_))))
		.map_with_span(|code, span| ParserStmt::Cpp(span, force_token!(code => StringLiteral)))
}

/// Parses a bare scope (not wrapped in curly braces) into ParserScope
pub fn bare_scope() -> token_parser!(ParserScope) {
	recursive(|scope| {
		stmt(scope, true)
			.repeated()
			.map_with_span(|x, span| ParserScope { span, stmts: x })
	})
}

pub fn parser() -> token_parser!(ParserScope) {
	bare_scope().then_ignore(end())
}

pub type CodeStream<'a> = Stream<'a, Token, Span, IntoIter<Spanned<Token>>>;

pub fn parse(
	code_stream: CodeStream,
) -> Result<ParserScope, (ParserScope, Vec<Diagnostic<usize>>)> {
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
	Err((
		parsed.unwrap_or_else(|| ParserScope {
			span: Span::new(0, 0..0),
			stmts: Vec::new(),
		}),
		diagnostics,
	))
}

// TODO: decide whether to change the function signature to allow returning functions with generics.
// i think it's pretty ugly to write `Node<T><T>(T) return_node_maker()`
//                                vs `func return_node_maker() -> func<T>(T) -> Node<T>`
// but maybe the cases are rare enough that we can ignore these problems.
