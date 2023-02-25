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
	parened!(ty_ident(None),)
}

/// Parses `<ty ident>(<ty ident>, ...) { <scope> }` into Stmt::Func
fn func_stmt(scope: ScopeRecursive) -> impl TokenParser<Stmt> + '_ {
	func_attribs()
		.then(ty_ident(None))
		.then(func_args())
		.then(choice((
			jkeyword!(FatArrow).ignore_then(expr()).map(|expr| Scope { stmts: vec![Stmt::Return(expr)] }),
			braced!(scope),
		)))
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
			atom()
				.then(parened!(e.clone(),).repeated())
				.foldl(|lhs, args| Expr::Call(Box::new(lhs), args))
		};
		let neg_parser = unop_parser!(Neg => fc_parser);
		let not_parser = unop_parser!(Bang => neg_parser);
		let eq_parser = binop_parser!(Eq Ne Lt Gt Le Ge => not_parser);
		let and_parser = binop_parser!(And => eq_parser);
		let or_parser = binop_parser!(Or => and_parser);
		let sd_parser = binop_parser!(Star Div => or_parser);
		let pn_parser = binop_parser!(Plus Neg => sd_parser);
		let lambda = || {
			parened!(choice((ty_ident(Some(e.clone())), ident().map(|x| x.infer_type()))),)
				.then(choice((
					// bare_scope(),
					jkeyword!(FatArrow)
						.ignore_then(pn_parser())
						.map(|expr| Scope {
							stmts: vec![Stmt::Return(expr)],
						}),
					jkeyword!(FatArrow)
						.ignore_then(pn_parser())
						.map(|expr| Scope {
							stmts: vec![Stmt::Return(expr)],
						}),
				)))
				.map(|(args, body)| {
					Expr::Lambda(Func {
						return_ty: Type::Inferred,
						body,
						args,
						attribs: FuncAttribs::default(),
					})
				})
		};
		choice((lambda(), pn_parser())).boxed()
	})
}

/// Parses an ident token into Ident
fn ident() -> impl TokenParser<Ident> {
	filter(|token| matches!(token, Token::Identifier(_)) || *token == keyword!(DontCare)).map(
		|token| {
			if token == keyword!(DontCare) {
				Ident::Discarded
			} else {
				force_token!(token => Identifier)
			}
		},
	)
}

/// Parses an ident token into Type
fn ty(er: Option<ExprRecursive>) -> impl TokenParser<Type> + '_ {
	type PostfixOp = Either<Option<Expr>, Operator>;
	recursive(|ty| {
		filter(|token| matches!(token, Token::Identifier(_)) || *token == keyword!(DontCare))
			.then(angled!(ty,).or_not())
			.map(|(ident, generics)| {
				if ident == keyword!(DontCare) {
					Type::Inferred
				} else {
					Type::BareType(BareType {
						ident: force_token!(ident => Identifier),
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
					bracketed!(er.map(|x| x.boxed()).unwrap_or_else(|| expr().boxed()).or_not()).map(PostfixOp::Left),
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
fn ty_ident(er: Option<ExprRecursive>) -> impl TokenParser<TypedIdent> + '_ {
	ty(er).then(ident())
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
	ty_ident(None)
		.then(assg!(noident ignore Set))
		.map(|(lhs, value)| Stmt::Create(lhs, value))
}

/// Parses `<expr>;` into Stmt::BareExpr
///
/// Useful, for example, for function calls where the return value is discarded
fn bare_expr_stmt() -> impl TokenParser<Stmt> {
	expr().map(Stmt::BareExpr)
}

fn return_stmt() -> impl TokenParser<Stmt> {
	jkeyword!(Return).ignore_then(expr()).map(Stmt::Return)
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
	recursive(|scope| stmt(scope, true).repeated().map(|x| Scope { stmts: x }))
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
						.with_message("this token is invalid")])
					.with_notes(vec![format!(
						"expected one of {}",
						err.expected()
							.map(|x| format!("'{}'", x.as_ref().unwrap_or(&Token::Error)))
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
