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

type ScopeRecursive<'a> = Recursive<'a, Token, Scope, Simple<Token, Span>>;

fn vec_ty_recovery() -> Vec<Type> {
	vec![builtin!(Error)]
}

fn ty_ident_recovery() -> TypedIdent {
	TypedIdent {
		ty: builtin!(Error),
		ident: ident!("error"),
	}
}

fn vec_ty_ident_recovery() -> Vec<TypedIdent> {
	vec![ty_ident_recovery()]
}

fn scope_recovery() -> Scope {
	Scope { stmts: vec![] }
}

fn expr_recovery() -> Expr {
	Expr::Error
}

fn opt_expr_recovery() -> Option<Expr> {
	None
}

fn vec_expr_recovery() -> Vec<Expr> {
	vec![Expr::Error]
}

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
								emit(chumsky::error::Simple::custom(span.clone(), "cannot apply attribute twice"))
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
		Pure => is_pure
	)
}

/// Parses `<ty ident>, ...` into Vec<TypedIdent>
fn func_args() -> impl TokenParser<Vec<TypedIdent>> {
	parened!(ty_ident(),; |_| vec_ty_ident_recovery())
}

/// Parses `<ty ident>(<ty ident>, ...) { <scope> }` into Stmt::Func
fn func_stmt(scope: ScopeRecursive) -> impl TokenParser<Stmt> + '_ {
	func_attribs()
		.then(ty_ident())
		.then(func_args())
		.then(braced!(scope; |_| scope_recovery()))
		.map(|(((attribs, ty_ident), args), body)| {
			Stmt::Func(
				ty_ident.ident,
				Func {
					return_ty: ty_ident.ty,
					args,
					body,
					attribs,
				},
			)
		})
}

// TODO: use this
#[allow(dead_code)]
/// Parses `func (<ty ident>, ...) -> <ty> { <scope> }` into Expr::Func
fn func_expr() -> impl TokenParser<Expr> {
	func_attribs()
		.then_ignore(jkeyword!(
			Function // TODO: will be renamed to Lambda, this will be a lambda function
		))
		.then(func_args())
		.then(braced!(parser(); |_| scope_recovery()))
		.map(|((attribs, args), scope)| {
			Expr::Func(Func {
				return_ty: builtin!(Void),
				args,
				body: scope,
				attribs,
			})
		})
}

/// Parses an expression into Expr
fn expr() -> impl TokenParser<Expr> {
	// () then - then ! then == != <= >= < > then && then || then *÷ then +-
	recursive(|e| {
		let atom = || {
			choice((
				parened!(e.clone(); |_| expr_recovery()),
				literal_parser!(StringLiteral),
				literal_parser!(NumberLiteral),
				literal_parser!(CharLiteral),
				ident()
					.then(parened!(e.clone(),; |_| vec_expr_recovery()))
					.map(|(ident, args)| Expr::Call(Box::new(Expr::Identifier(ident)), args)),
				// func_expr()
				// 	.then(
				// 		e.clone()
				// 			.separated_by(jpunct!(Comma))
				// 			.delimited_by(jpunct!(LParen), jpunct!(RParen)),
				// 	)
				// 	.map(|(func, args)| Expr::Call(Box::new(func), args)),
			))
		};
		let neg_parser = unop_parser!(Neg => atom);
		let not_parser = unop_parser!(Bang => neg_parser);
		let eq_parser = binop_parser!(Eq Ne Lt Gt Le Ge => not_parser);
		let and_parser = binop_parser!(And => eq_parser);
		let or_parser = binop_parser!(Or => and_parser);
		let sd_parser = binop_parser!(Star Div => or_parser);
		let pn_parser = binop_parser!(Plus Neg => sd_parser);
		pn_parser().boxed()
	})
}

/// Parses an ident token into Ident
fn ident() -> impl TokenParser<Ident> {
	filter(|token| matches!(token, Token::Identifier(_)) || *token == keyword!(DontCare)).map(
		|token| {
			if token == keyword!(DontCare) {
				Ident::Discarded
			} else {
				Ident::Named(force_token!(token => Identifier))
			}
		},
	)
}

/// Parses an ident token into Type
fn ty() -> impl TokenParser<Type> {
	type PostfixOp = Either<Option<Expr>, Operator>;
	recursive(|ty| {
		filter(|token| matches!(token, Token::Identifier(_)) || *token == keyword!(DontCare))
			.then(angled!(ty,; |_| vec_ty_recovery()).or_not())
			.map(|(ident, generics)| {
				if ident == keyword!(DontCare) {
					Type::Inferred
				} else {
					Type::BareType(BareType {
						ident: Ident::Named(force_token!(ident => Identifier)),
						generics: generics
							.unwrap_or(vec![])
							.iter()
							.map(|x: &Type| Generic::Type(x.clone()))
							.collect(),
					})
				}
			})
			.then(
				choice((
					bracketed!(expr().or_not(); |_| opt_expr_recovery()).map(PostfixOp::Left),
					jop!(Question).map(|x| PostfixOp::Right(force_token!(x => Operator))),
					jop!(Amp).map(|x| PostfixOp::Right(force_token!(x => Operator))),
					jop!(And).map(|x| PostfixOp::Right(force_token!(x => Operator))),
				))
				.repeated(),
			)
			.foldl(|ty, new_info| match new_info {
				PostfixOp::Left(x) => Type::Array(Box::new(ty), x.map(Box::new)),
				PostfixOp::Right(x) => match x {
					Operator::Question => Type::Optional(Box::new(ty)),
					Operator::Amp => Type::Ref(Box::new(ty)),
					Operator::And => Type::Ref(Box::new(Type::Ref(Box::new(ty)))),
					_ => unreachable!(),
				},
			})
	})
}

/// Parses `<ty> <ident>` into TypedIdent
fn ty_ident() -> impl TokenParser<TypedIdent> {
	ty().then(ident())
		.map(|(ty, ident)| TypedIdent { ty, ident })
}

/// Parses `let <ident> = <expr>;` into Stmt::Let
fn let_stmt() -> impl TokenParser<Stmt> {
	jkeyword!(Let)
		.ignore_then(assg!(ignore Set))
		.map(|(lhs, value): (Ident, Expr)| Stmt::Create(lhs.infer_type(), value))
}

/// Parses `<ty ident> = <expr>;` into Stmt::Create
fn create_stmt() -> impl TokenParser<Stmt> {
	ty_ident()
		.then(assg!(noident ignore Set))
		.map(|(lhs, value)| Stmt::Create(lhs, value))
}

/// Parses `<expr>;` into Stmt::BareExpr
///
/// Useful, for example, for function calls where the return value is discarded
fn bare_expr_stmt() -> impl TokenParser<Stmt> {
	expr().then_ignore(jpunct!(Semicolon)).map(Stmt::BareExpr)
}

/// Parses a bare scope (not wrapped in curly braces) into Scope
pub fn parser() -> impl TokenParser<Scope> {
	recursive(|scope| {
		choice((
			let_stmt(),
			create_stmt(),
			func_stmt(scope),
			assg_stmt!(Set),
			assg_stmt!(NegSet => Neg),
			assg_stmt!(StarSet => Star),
			assg_stmt!(PlusSet => Plus),
			assg_stmt!(DivSet => Div),
			bare_expr_stmt(),
		))
		.repeated()
		.map(|x| Scope { stmts: x })
	})
	.then_ignore(end())
}

pub type CodeStream<'a> = Stream<'a, Token, Span, IntoIter<Spanned<Token>>>;

pub fn parse(code_stream: CodeStream) -> Result<Scope, Vec<Diagnostic<usize>>> {
	let (parsed, errors) = parser().parse_recovery(code_stream);
	let mut diagnostics = vec![];
	if errors.is_empty() {
		return Ok(parsed.expect("what"));
	}
	for err in errors {
		match err.reason() {
			SimpleReason::Unclosed { span, delimiter } => diagnostics.push(
				Diagnostic::error()
					.with_message(format!("unclosed delimiter {delimiter}"))
					.with_labels(vec![
						Label::primary(err.span().file_id, err.span().range())
							.with_message("invalid delimiter"),
						Label::secondary(span.file_id, span.range())
							.with_message("opening delimiter here"),
					]),
			),
			SimpleReason::Unexpected => diagnostics.push(
				Diagnostic::error()
					.with_message("unexpected token")
					.with_labels(vec![Label::primary(err.span().file_id, err.span().range())
						.with_message(format!("this token is invalid"))])
					.with_notes(vec![format!(
						"expected one of {}",
						err.expected()
							.map(|x| format!("'{}'", x.as_ref().unwrap()))
							.reduce(|acc, b| acc + ", " + &b)
							.unwrap_or("".into())
					)]),
			),
			SimpleReason::Custom(label) => diagnostics.push(
				Diagnostic::error()
					.with_message(label)
					.with_labels(vec![Label::primary(err.span().file_id, err.span().range())]),
			),
		}
	}
	Err(diagnostics)
}
