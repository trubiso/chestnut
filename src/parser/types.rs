use crate::lexer::{NumberLiteral, Operator, Token};
use crate::span::Span;
use chumsky::prelude::*;
use derive_more::Display;
use std::fmt;

pub trait TokenParser<T> = Parser<Token, T, Error = Simple<Token, Span>>;

#[derive(Debug, Display, Clone)]
pub enum Generic {
	Type(Type),
	Expr(Expr), // TODO: remove the additional junk caused by this mistake of an enum
}

#[derive(Debug, Display, Clone, PartialEq, Eq)]
pub enum Ident {
	#[display(fmt = "{_1}")]
	Named(Span, String),
	#[display(fmt = "~")]
	Discarded(Span),
}

impl Ident {
	pub fn span(&self) -> Span {
		match self.clone() {
			Self::Named(x, _) => x,
			Self::Discarded(x) => x,
		}
	}

	pub fn infer_type(&self) -> TypedIdent {
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

#[derive(Debug, Clone)]
pub struct BareType {
	pub ident: Ident,           // typename
	pub generics: Vec<Generic>, // generics
}

#[derive(Debug, Display, Clone)]
pub enum BuiltinType {
	#[display(fmt = "void")]
	Void,
	#[display(fmt = "error")]
	Error,
}

#[derive(Debug, Clone)]
pub enum Type {
	BareType(Span, BareType),
	Builtin(Span, BuiltinType),
	Array(Span, Box<Type>, Option<Box<Expr>>),
	Ref(Span, Box<Type>),
	Optional(Span, Box<Type>),
	Mut(Span, Box<Type>),
	Inferred(Span),
}

impl Type {
	pub fn span(&self) -> Span {
		match self {
			Self::BareType(x, _)
			| Self::Builtin(x, _)
			| Self::Array(x, _, _)
			| Self::Ref(x, _)
			| Self::Optional(x, _)
			| Self::Mut(x, _)
			| Self::Inferred(x) => x.clone(),
		}
	}
}

#[derive(Debug, Display, Clone)]
#[display(fmt = "{ty} {ident}")]
pub struct TypedIdent {
	pub span: Span,
	pub ty: Type,
	pub ident: Ident,
}

impl TypedIdent {
	pub fn ident_str(&self) -> String {
		self.ident.to_string()
	}
}

#[derive(Debug, Display, Clone, PartialEq, Eq)]
pub enum Privacy {
	#[display(fmt = "private ")]
	Private,
	#[display(fmt = "protected ")]
	Protected,
	#[display(fmt = "public ")]
	Public,
	#[display(fmt = "export ")]
	Export,
	#[display(fmt = "")]
	Default,
}

#[derive(Debug, Clone, Default)]
pub struct FuncAttribs {
	pub is_pure: bool,
	pub is_mut: bool,
}

#[derive(Debug, Clone)]
pub struct Func {
	pub span: Span,
	pub return_ty: Type,
	pub args: Vec<TypedIdent>, // TODO: perhaps we might need to change this
	pub body: Scope,
	pub attribs: FuncAttribs,
}

#[derive(Debug, Clone)]
pub enum Expr {
	CharLiteral(Span, String),
	StringLiteral(Span, String),
	NumberLiteral(Span, NumberLiteral),
	Identifier(Span, Ident),
	BinaryOp(Span, Box<Expr>, Operator, Box<Expr>),
	UnaryOp(Span, Operator, Box<Expr>),
	Lambda(Span, Func),
	Call(Span, Box<Expr>, Vec<Expr>),
	Error(Span),
}

impl Expr {
	pub fn span(&self) -> Span {
		match self {
			Self::CharLiteral(x, _)
			| Self::StringLiteral(x, _)
			| Self::NumberLiteral(x, _)
			| Self::Identifier(x, _)
			| Self::BinaryOp(x, _, _, _)
			| Self::UnaryOp(x, _, _)
			| Self::Lambda(x, _)
			| Self::Call(x, _, _)
			| Self::Error(x) => x.clone(),
		}
	}
}

#[derive(Debug, Display, Clone)]
pub enum Stmt {
	#[display(fmt = "{_1}{_2} = {_3}")]
	Create(Span, Privacy, TypedIdent, Expr),
	#[display(fmt = "declare {_1}{_2}")]
	Declare(Span, Privacy, TypedIdent),
	#[display(fmt = "{_1} = {_2}")]
	Set(Span, Ident, Expr),
	#[display(fmt = "{_1}{_2} = {_3}")]
	Func(Span, Privacy, Ident, Func),
	#[display(fmt = "return {_1}")]
	Return(Span, Expr),
	#[display(fmt = "class {_1}{_2} {{{_3}}}")]
	Class(Span, Privacy, Ident, Scope),
	#[display(fmt = "{_1}")]
	BareExpr(Span, Expr),
}

#[derive(Debug, Clone)]
pub struct Scope {
	pub span: Span,
	pub stmts: Vec<Stmt>,
}

impl fmt::Display for BareType {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		f.write_fmt(format_args!(
			"{}{}",
			self.ident,
			match join_comma(&self.generics) {
				None => "".to_string(),
				Some(x) => format!("<{x}>"),
			},
		))
	}
}

impl fmt::Display for Type {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Type::BareType(_, x) => f.write_fmt(format_args!("{x}")),
			Type::Builtin(_, x) => f.write_fmt(format_args!("{x}")),
			Type::Array(_, x, len) => f.write_fmt(format_args!(
				"{x}{}",
				if let Some(len) = len {
					format!("[{len}]")
				} else {
					"[]".to_string()
				}
			)),
			Type::Ref(_, x) => f.write_fmt(format_args!("{x}&")),
			Type::Optional(_, x) => f.write_fmt(format_args!("{x}?")),
			Type::Mut(_, x) => f.write_fmt(format_args!("{x} mut")),
			Type::Inferred(_) => f.write_str("~"),
		}
	}
}

impl fmt::Display for FuncAttribs {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		f.write_fmt(format_args!(
			"{}{}",
			if self.is_pure { "pure " } else { "" },
			if self.is_mut { "mut " } else { "" },
		))
	}
}

impl fmt::Display for Func {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		f.write_fmt(format_args!(
			"func ({}) {}-> {}: {{{}}}",
			join_comma(&self.args).unwrap_or("".into()),
			self.attribs,
			self.return_ty,
			if self.body.stmts.is_empty() {
				"".into()
			} else {
				format!(
					"\n{}",
					format!("{}", self.body)
						.split('\n')
						.map(|x: &str| -> String { format!("\t{}\n", x) })
						.reduce(|acc, b| acc + &b)
						.unwrap()
				)
			},
		))
	}
}

impl fmt::Display for Expr {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Expr::CharLiteral(_, x) => f.write_fmt(format_args!("{x}")),
			Expr::StringLiteral(_, x) => f.write_fmt(format_args!("{x}")),
			Expr::NumberLiteral(_, x) => f.write_fmt(format_args!("{x}")),
			Expr::Identifier(_, x) => f.write_fmt(format_args!("{x}")),
			Expr::BinaryOp(_, lhs, op, rhs) => f.write_fmt(format_args!("({lhs} {op} {rhs})")),
			Expr::UnaryOp(_, op, expr) => f.write_fmt(format_args!("({op}{expr})")),
			Expr::Lambda(_, func) => f.write_fmt(format_args!("lambda {func}")),
			Expr::Call(_, callee, args) => f.write_fmt(format_args!(
				"{callee}({})",
				join_comma(args).unwrap_or("".to_string())
			)),
			Expr::Error(_) => f.write_str("[ERROR]"),
		}
	}
}

impl fmt::Display for Scope {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		for stmt in &self.stmts {
			f.write_fmt(format_args!("{stmt}\n"))?;
		}
		Ok(())
	}
}

fn join_comma<T: fmt::Display>(vec: &[T]) -> Option<String> {
	vec.iter()
		.map(|x| format!("{x}"))
		.reduce(|acc, b| acc + ", " + &b)
}
