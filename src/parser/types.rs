use crate::common::{Expr, Func, Scope, ScopeFmt, Stmt, Type, TypedIdent, BareType, FuncSignature};
use crate::lexer::Token;
use crate::span::Span;
use chumsky::prelude::*;
use std::fmt;

pub type TokenRecursive<'a, T> = Recursive<'a, Token, T, Simple<Token, Span>>;
pub type ScopeRecursive<'a> = TokenRecursive<'a, ParserScope>;
pub type ExprRecursive<'a> = TokenRecursive<'a, ParserExpr>;

pub type ParserExpr = Expr<ParserScope>;
pub type ParserType = Type<ParserExpr>;
pub type ParserTypedIdent = TypedIdent<ParserType>;
pub type ParserFunc = Func<ParserExpr, ParserScope>;
pub type ParserFuncSignature = FuncSignature<ParserType>;
pub type ParserStmt = Stmt<ParserExpr, ParserFunc, ParserScope>;
pub type ParserBareType = BareType<ParserType>;

// TODO: go on each PartialEq with spans and re-implement it manually

#[derive(Debug, Clone)]
pub enum Ident {
	Named(Span, String),
	Qualified(Span, Vec<Ident>),
	Discarded(Span),
}

impl fmt::Display for Ident {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Self::Named(_, x) => f.write_str(x),
			Self::Qualified(_, x) => f.write_str(
				&x.iter()
					.map(|x| format!("{x}"))
					.reduce(|acc, b| acc + "::" + &b)
					.unwrap(),
			),
			Self::Discarded(_) => f.write_str("~"),
		}
	}
}

impl PartialEq for Ident {
	fn eq(&self, other: &Self) -> bool {
		match self {
			Self::Named(_, x) => {
				if let Self::Named(_, y) = other {
					x == y
				} else {
					false
				}
			}
			Self::Qualified(_, x) => {
				if let Self::Qualified(_, y) = other {
					x == y
				} else {
					false
				}
			}
			Self::Discarded(_) => matches!(other, Self::Discarded(_)),
		}
	}
}

impl Eq for Ident {}

impl Ident {
	pub fn span(&self) -> Span {
		match self.clone() {
			Self::Named(x, _) => x,
			Self::Qualified(x, _) => x,
			Self::Discarded(x) => x,
		}
	}

	pub fn infer_type<Expr: fmt::Display>(&self) -> TypedIdent<Type<Expr>> {
		TypedIdent {
			span: self.span(),
			ty: Type::Inferred(self.span()),
			ident: self.clone(),
		}
	}

	pub fn is_discarded(&self) -> bool {
		matches!(self, Ident::Discarded(_))
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParserScope {
	pub span: Span,
	pub stmts: Vec<ParserStmt>,
}

impl Scope<Expr<ParserScope>> for ParserScope {
	fn braced(&self) -> String {
		let body = format!("{}", self);
		format!(
			"{{{}}}",
			if self.stmts.is_empty() {
				"".into()
			} else {
				format!("\n{}", {
					let mut x: Vec<_> = body.split('\n').collect();
					x.pop();
					x.iter()
						.map(|x| -> String { format!("\t{}\n", x) })
						.reduce(|acc, b| acc + &b)
						.unwrap()
				})
			},
		)
	}

	fn stmts(&self) -> &Vec<Stmt<Expr<ParserScope>, Func<Expr<ParserScope>, Self>, Self>>
	where
		Self: Sized,
	{
		&self.stmts
	}
}

impl fmt::Display for ParserScope {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		self.scope_fmt(f)
	}
}

pub fn join_comma<T: fmt::Display>(vec: &[T]) -> Option<String> {
	vec.iter()
		.map(|x| format!("{x}"))
		.reduce(|acc, b| acc + ", " + &b)
}
