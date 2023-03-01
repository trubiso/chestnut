use crate::{
	lexer::{AssignmentOp, Keyword, Operator, Punctuation, Token},
	span::{Span, Spanned},
};
use chumsky::{error::SimpleReason, prelude::*, Stream};
use codespan_reporting::diagnostic::{Diagnostic, Label};
use either::Either;
use std::vec::IntoIter;
use types::*;

pub mod types;
#[macro_use]
pub mod macros;

type TokenRecursive<'a, T> = Recursive<'a, Token, T, Simple<Token, Span>>;
type ScopeRecursive<'a> = TokenRecursive<'a, Scope>;
type ExprRecursive<'a> = TokenRecursive<'a, Expr>;

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
							if Keyword::$kw == Keyword::Private
							|| Keyword::$kw == Keyword::Protected
							|| Keyword::$kw == Keyword::Public {
								if final_attribs.is_private
								|| final_attribs.is_protected
								|| final_attribs.is_public {
									emit(chumsky::error::Simple::custom(span.clone(), "too many privacy qualifiers"));
								}
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

fn func_attribs() -> impl TokenParser<FuncAttribs> {
	func_attribs!(
		Private => is_private
		Protected => is_protected
		Public => is_public
		Pure => is_pure
	)
}

/// Parses `<ty ident>, ...` into Vec<TypedIdent>
fn func_args() -> impl TokenParser<Vec<TypedIdent>> {
	parened!(ty_ident(None),)
}

/// Parses `<ty ident>(<ty ident>, ...) { <scope> }` into Stmt::Func
fn func_stmt(scope: ScopeRecursive) -> impl TokenParser<Stmt> + '_ {
	func_attribs()
		.then(ty_ident(None))
		.then(func_args())
		.then(choice((
			jkeyword!(FatArrow)
				.ignore_then(expr())
				.map_with_span(|expr, span| Scope {
					span: span.clone(),
					stmts: vec![Stmt::Return(span, expr)],
				}),
			braced!(scope),
		)))
		.map_with_span(|(((attribs, ty_ident), args), body), span| {
			Stmt::Func(
				span.clone(),
				ty_ident.ident,
				Func {
					span,
					return_ty: ty_ident.ty,
					args,
					body,
					attribs,
				},
			)
		})
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
						args,
						attribs: FuncAttribs::default(),
					},
				)
			})
		};
		choice((lambda(), pn_parser())).boxed()
	})
}

/// Parses an ident token into Ident
fn ident() -> impl TokenParser<Ident> {
	filter(|token| matches!(token, Token::Identifier(_)) || *token == keyword!(DontCare))
		.map_with_span(|token, span| {
			if token == keyword!(DontCare) {
				Ident::Discarded(span)
			} else {
				force_token!(token => Identifier, span)
			}
		})
}

/// Parses an ident token into Type
fn ty(er: Option<ExprRecursive>) -> impl TokenParser<Type> + '_ {
	type PostfixOp = Either<Option<Expr>, Operator>;
	recursive(|ty| {
		filter(|token| matches!(token, Token::Identifier(_)) || *token == keyword!(DontCare))
			.then(angled!(ty,).or_not())
			.map_with_span(|(ident, generics), span| {
				if ident == keyword!(DontCare) {
					Type::Inferred(span)
				} else {
					Type::BareType(
						span.clone(),
						BareType {
							ident: force_token!(ident => Identifier, span), // TODO: incorrect span
							generics: generics
								.unwrap_or(vec![])
								.iter()
								.map(|x: &Type| Generic::Type(x.clone()))
								.collect(),
						},
					)
				}
			})
			.then(
				choice((
					bracketed!(er
						.map(|x| x.boxed())
						.unwrap_or_else(|| expr().boxed())
						.or_not())
					.map(PostfixOp::Left),
					jop!(Question).map(|x| PostfixOp::Right(force_token!(x => Operator))),
					jop!(Amp).map(|x| PostfixOp::Right(force_token!(x => Operator))),
					jop!(And).map(|x| PostfixOp::Right(force_token!(x => Operator))),
				))
				.map_with_span(|x, s| (x, s))
				.repeated(),
			)
			.foldl(|ty, (new_info, span)| match new_info {
				PostfixOp::Left(x) => Type::Array(span, Box::new(ty), x.map(Box::new)),
				PostfixOp::Right(x) => match x {
					Operator::Question => Type::Optional(span, Box::new(ty)),
					Operator::Amp => Type::Ref(span, Box::new(ty)),
					Operator::And => {
						Type::Ref(span.clone(), Box::new(Type::Ref(span, Box::new(ty))))
					}
					// TODO: merge span with ty.span, all of these spans are wrong LOL
					_ => unreachable!(),
				},
			})
	})
}

/// Parses `<ty> <ident>` into TypedIdent
fn ty_ident(er: Option<ExprRecursive>) -> impl TokenParser<TypedIdent> + '_ {
	ty(er)
		.then(ident())
		.map_with_span(|(ty, ident), span| TypedIdent { span, ty, ident })
}

/// Parses `let <ident> = <expr>;` into Stmt::Let
fn let_stmt() -> impl TokenParser<Stmt> {
	jkeyword!(Let).ignore_then(assg!(ignore Set)).map_with_span(
		|(lhs, value): (Ident, Expr), span| Stmt::Create(span, lhs.infer_type(), value),
	)
}

/// Parses `<ty ident> = <expr>;` into Stmt::Create
fn create_stmt() -> impl TokenParser<Stmt> {
	ty_ident(None)
		.then(assg!(noident ignore Set))
		.map_with_span(|(lhs, value), span| Stmt::Create(span, lhs, value))
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
		semi!(N func_stmt(scope)),
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
