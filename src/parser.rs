use crate::lexer::{AssignmentOp, Keyword, Operator, Punctuation, Spanned, Token};
use chumsky::{error::SimpleReason, prelude::*, Stream};
use codespan_reporting::diagnostic::{Diagnostic, Label};
use either::Either;
use std::{ops::Range, vec::IntoIter};
use types::*;

pub mod types;
#[macro_use]
pub mod macros;

type ScopeRecursive<'a> = Recursive<'a, Token, Scope, Simple<Token>>;

fn func_args() -> impl TokenParser<Vec<TypedIdent>> {
	ty_ident()
		.separated_by(jpunct!(Comma))
		.delimited_by(jpunct!(LParen), jpunct!(RParen))
}

/// Parses a function of the format `<ty_ident>(<ty ident>, ...) { <scope> }`
fn func_stmt(scope: ScopeRecursive) -> impl TokenParser<Stmt> + '_ {
	ty_ident()
		.then(func_args())
		.then(scope.delimited_by(jpunct!(LBrace), jpunct!(RBrace)))
		.map(|((ty_ident, args), body)| {
			Stmt::Func(
				ty_ident.ident,
				Func {
					return_ty: ty_ident.ty,
					args,
					body,
				},
			)
		})
}

/// Parses a function of the format `func <ident>(<ty ident>, ...) -> <ty> {
/// <scope> }`
fn func_stmt_alt(scope: ScopeRecursive) -> impl TokenParser<Stmt> + '_ {
	jkeyword!(Function)
		.ignore_then(ident())
		.then(func_args())
		.then(jkeyword!(Arrow).ignore_then(ty()).or_not())
		.then(scope.delimited_by(jpunct!(LBrace), jpunct!(RBrace)))
		.map(|(((ident, args), ty), body)| {
			Stmt::Func(
				ident,
				Func {
					return_ty: ty.unwrap_or(builtin!(Void)),
					args,
					body,
				},
			)
		})
}

// TODO: use this
#[allow(dead_code)]
fn func_expr() -> impl TokenParser<Expr> {
	jkeyword!(Function)
		.ignore_then(func_args())
		.then(parser().delimited_by(jpunct!(LBrace), jpunct!(RBrace)))
		.map(|(args, scope)| {
			Expr::Func(Func {
				return_ty: Type::BareType(BareType {
					ident: ident!("void"),
					generics: vec![],
				}), // TODO: this is stupid
				args,
				body: scope,
			})
		})
}

fn expr() -> impl TokenParser<Expr> {
	// () then - then ! then == != <= >= < > then && then || then *÷ then +-
	recursive(|e| {
		let atom = || {
			choice((
				e.clone().delimited_by(jpunct!(LParen), jpunct!(RParen)),
				literal_parser!(StringLiteral),
				literal_parser!(NumberLiteral),
				literal_parser!(CharLiteral),
				ident()
					.then(
						e.clone()
							.separated_by(jpunct!(Comma))
							.delimited_by(jpunct!(LParen), jpunct!(RParen)),
					)
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

fn ident() -> impl TokenParser<Ident> {
	filter(|x| matches!(x, Token::Identifier(_))).map(|x| Ident(force_token!(x => Identifier)))
}

fn ty() -> impl TokenParser<Type> {
	type PostfixOp = Either<Option<Expr>, Operator>;
	recursive(|ty| {
		filter(|x| matches!(x, Token::Identifier(_)))
			.then(
				ty.separated_by(jpunct!(Comma))
					.delimited_by(jop!(Lt), jop!(Gt))
					.or_not(),
			)
			.map(|(ident, generics)| {
				Type::BareType(BareType {
					ident: Ident(force_token!(ident => Identifier)),
					generics: generics
						.unwrap_or(vec![])
						.iter()
						.map(|x: &Type| Generic::Type(x.clone()))
						.collect(),
				})
			})
			.then(
				choice((
					expr()
						.or_not()
						.delimited_by(jpunct!(LBracket), jpunct!(RBracket))
						.map(PostfixOp::Left),
					jop!(Question).map(|x| PostfixOp::Right(force_token!(x => Operator))),
					jop!(Amp).map(|x| PostfixOp::Right(force_token!(x => Operator))),
					jop!(And).map(|x| PostfixOp::Right(force_token!(x => Operator))),
				))
				.repeated(),
			)
			.foldl(|mut ty, new_info| match new_info {
				PostfixOp::Left(x) => Type::Array(Box::new(ty), x.map(Box::new)),
				PostfixOp::Right(x) => {
					match x {
						Operator::Question => ty = Type::Optional(Box::new(ty)),
						Operator::Amp => ty = Type::Ref(Box::new(ty)),
						Operator::And => ty = Type::Ref(Box::new(Type::Ref(Box::new(ty)))),
						_ => unreachable!(),
					}
					ty
				}
			})
	})
}

fn ty_ident() -> impl TokenParser<TypedIdent> {
	ty().then(ident())
		.map(|(ty, ident)| TypedIdent { ty, ident })
}

fn let_stmt() -> impl TokenParser<Stmt> {
	jkeyword!(Let)
		.ignore_then(assg!(ignore Set))
		.map(|(lhs, value)| Stmt::Let(lhs, value))
}

fn create_stmt() -> impl TokenParser<Stmt> {
	ty_ident()
		.then(assg!(noident ignore Set))
		.map(|(lhs, value)| Stmt::Create(lhs, value))
}

fn bare_expr_stmt() -> impl TokenParser<Stmt> {
	expr().then_ignore(jpunct!(Semicolon)).map(Stmt::BareExpr)
}

pub fn parser() -> impl TokenParser<Scope> {
	recursive(|scope| {
		choice((
			let_stmt(),
			create_stmt(),
			func_stmt(scope.clone()),
			func_stmt_alt(scope),
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
}

type CodeStream<'a> = Stream<'a, Token, Range<usize>, IntoIter<Spanned<Token>>>;

pub fn parse(code_stream: CodeStream, file_id: usize) -> Result<Scope, Vec<Diagnostic<usize>>> {
	let parsed = parser().parse(code_stream);
	match parsed {
		Ok(scope) => Ok(scope),
		Err(errors) => {
			let mut diagnostics = vec![];
			for err in errors {
				match err.reason() {
					SimpleReason::Unexpected => diagnostics.push(
						Diagnostic::error()
							.with_message("unexpected token")
							.with_labels(vec![Label::primary(file_id, err.span())
								.with_message("this token is invalid")]),
					),
					SimpleReason::Unclosed { span, delimiter } => diagnostics.push(
						Diagnostic::error()
							.with_message(format!("unclosed delimiter {delimiter:?}"))
							.with_labels(vec![
								Label::primary(file_id, span.clone()).with_message("culprit")
							]),
					),
					_ => panic!("unhandled error"),
				}
			}
			Err(diagnostics)
		}
	}
}
