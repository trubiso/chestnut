use crate::{
	lexer::{Keyword, Operator, Token},
	span::{Span, Spanned},
};
use chumsky::{error::SimpleReason, prelude::*, Stream};
use codespan_reporting::diagnostic::{Diagnostic, Label};
use ident::*;
use std::vec::IntoIter;
use ty_ident::*;
use types::*;

pub mod types;
#[macro_use]
pub mod macros;
mod ident;
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

macro_rules! privacy_qualifiers {
	($($kw:ident)*) => {
		choice(($(jkeyword!($kw),)*))
		.repeated()
		.validate(|attribs, span: Span, emit| {
			if attribs.len() > 1 {
				emit(chumsky::error::Simple::custom(span.clone(), "too many privacy qualifiers"));
			}

			if let Some(x) = attribs.get(0) {
				match force_token!(x => Keyword) {
					$(Keyword::$kw => Privacy::$kw(span),)*
					_ => unreachable!()
				}
			} else {
				Privacy::Default
			}
		})
	};
}

fn privacy_attribs() -> impl TokenParser<Privacy> {
	privacy_qualifiers!(Private Protected Public Export)
}

fn func_attribs() -> impl TokenParser<FuncAttribs> {
	func_attribs!(
		Pure => is_pure
		Mut => is_mut
	)
}

/// Parses `<ty ident>, ...` into Vec<TypedIdent>
fn func_args() -> impl TokenParser<Vec<TypedIdent>> {
	parened!(ty_ident(None),)
}

/// Parses `<<ident>, ...>` into Vec<Ident>
fn generics_declare() -> impl TokenParser<Vec<Ident>> {
	angled!(ident_nodiscard(),)
		.or_not()
		.map(|x| x.unwrap_or_else(Vec::new))
}

/// Parses `<ty ident>(<ty ident>, ...) { <scope> }` into Stmt::Func
fn func_stmt(scope: ScopeRecursive) -> impl TokenParser<Stmt> + '_ {
	privacy_attribs()
		.then(ty_ident_nodiscard(None))
		.then(generics_declare())
		.then(func_args())
		.then(func_attribs())
		.then(choice((
			jkeyword!(FatArrow)
				.ignore_then(expr())
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

/// Parses an expression into Expr
fn expr() -> impl TokenParser<Expr> {
	// () then - then ! then == != <= >= < > then && then || then *÷ then +-
	recursive(|e| {
		let atom = || {
			choice((
				parened!(e.clone()),
				literal_parser!(StringLiteral),
				literal_parser!(NumberLiteral),
				literal_parser!(CharLiteral),
				literal_parser!(Identifier),
			))
		};
		let fc_parser = || {
			span!(atom())
				.then(span!(parened!(e.clone(),)).repeated())
				.foldl(|(lhs, ls), (args, rs)| {
					let span: Span = ls + rs;
					(Expr::Call(span.clone(), Box::new(lhs), args), span)
				})
				.map(|(x, _)| x)
		};
		let neg_parser = unop_parser!(Neg => fc_parser);
		let not_parser = unop_parser!(Bang => neg_parser);
		let eq_parser = binop_parser!(Eq Ne Lt Gt Le Ge => not_parser);
		let and_parser = binop_parser!(And => eq_parser);
		let or_parser = binop_parser!(Or => and_parser);
		let sd_parser = binop_parser!(Star Div => or_parser);
		let pn_parser = binop_parser!(Plus Neg => sd_parser);
		let lambda = || {
			parened!(choice((
				ty_ident(Some(e.clone())),
				ident().map(|x| x.infer_type())
			)),)
			.then(choice((
				// bare_scope(),
				jkeyword!(FatArrow)
					.ignore_then(pn_parser())
					.map_with_span(|expr, span| Scope {
						span: span.clone(),
						stmts: vec![Stmt::Return(span, expr)],
					}),
				jkeyword!(FatArrow)
					.ignore_then(pn_parser())
					.map_with_span(|expr, span| Scope {
						span: span.clone(),
						stmts: vec![Stmt::Return(span, expr)],
					}),
			)))
			.map_with_span(|(args, body), span| {
				Expr::Lambda(
					span.clone(),
					Func {
						span: span.clone(),
						return_ty: Type::Inferred(span), // NOTE: there's no span for this type
						body,
						generics: Vec::new(), // NOTE: should lambda generics?
						args,
						attribs: FuncAttribs::default(),
					},
				)
			})
		};
		choice((lambda(), pn_parser())).boxed()
	})
}

/// Parses `let <ident> = <expr>;` into Stmt::Let
fn let_stmt() -> impl TokenParser<Stmt> {
	privacy_attribs()
		.then_ignore(jkeyword!(Let))
		.then(assg!(ignore Set))
		.map_with_span(|(privacy, (lhs, value)), span| {
			Stmt::Create(span, privacy, lhs.infer_type(), value)
		})
}

/// Parses `<ty ident> = <expr>;` into Stmt::Create
fn create_stmt() -> impl TokenParser<Stmt> {
	privacy_attribs()
		.then(ty_ident(None))
		.then(assg!(noident ignore Set))
		.map_with_span(|((privacy, lhs), value), span| Stmt::Create(span, privacy, lhs, value))
}

/// Parses `<ty ident>;` into Stmt::Declare
fn declare_stmt() -> impl TokenParser<Stmt> {
	privacy_attribs()
		.then(ty_ident_nodiscard(None))
		.map_with_span(|(privacy, ty_ident), span| Stmt::Declare(span, privacy, ty_ident))
}

/// Parses `<expr>;` into Stmt::BareExpr
///
/// Useful, for example, for function calls where the return value is discarded
fn bare_expr_stmt() -> impl TokenParser<Stmt> {
	expr().map_with_span(|x, span| Stmt::BareExpr(span, x))
}

fn return_stmt() -> impl TokenParser<Stmt> {
	jkeyword!(Return)
		.ignore_then(expr())
		.map_with_span(|x, span| Stmt::Return(span, x))
}

fn class_stmt(scope: ScopeRecursive) -> impl TokenParser<Stmt> + '_ {
	privacy_attribs()
		.then_ignore(jkeyword!(Class))
		.then(ident_nodiscard())
		.then(generics_declare())
		.then(braced!(scope))
		.map_with_span(|(((privacy, ident), generics), body), span| {
			Stmt::Class(span, privacy, ident, generics, body)
		})
}

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
		semi!(Y create_stmt()),
		semi!(Y declare_stmt()),
		semi!(N func_stmt(scope.clone())),
		semi!(N class_stmt(scope)),
		semi!(Y return_stmt()),
		semi!(Y assg_stmt!(Set)),
		semi!(Y assg_stmt!(NegSet => Neg)),
		semi!(Y assg_stmt!(StarSet => Star)),
		semi!(Y assg_stmt!(PlusSet => Plus)),
		semi!(Y assg_stmt!(DivSet => Div)),
		semi!(Y bare_expr_stmt()),
	))
}

/// Parses a bare scope (not wrapped in curly braces) into Scope
pub fn bare_scope() -> impl TokenParser<Scope> {
	recursive(|scope| {
		stmt(scope, true)
			.repeated()
			.map_with_span(|x, span| Scope { span, stmts: x })
	})
}

pub fn parser() -> impl TokenParser<Scope> {
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
