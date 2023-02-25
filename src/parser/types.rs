use crate::lexer::{NumberLiteral, Operator, Token};
use crate::span::Span;
use chumsky::prelude::*;
use derive_more::Display;
use std::fmt;

pub trait TokenParser<T> = Parser<Token, T, Error = Simple<Token, Span>>;

#[derive(Debug, Display, Clone)]
pub enum Generic {
	Type(Type),
	Expr(Expr),
}

#[derive(Debug, Display, Clone, PartialEq, Eq)]
pub enum Ident {
	#[display(fmt = "{_0}")]
	Named(String),
	#[display(fmt = "~")]
	Discarded,
}

impl Ident {
	pub fn infer_type(&self) -> TypedIdent {
		TypedIdent {
			ty: Type::Inferred,
			ident: self.clone(),
		}
	}

	pub fn is_discarded(&self) -> bool {
		*self == Ident::Discarded
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
	BareType(BareType),
	Builtin(BuiltinType),
	Array(Box<Type>, Option<Box<Expr>>),
	Ref(Box<Type>),
	Optional(Box<Type>),
	Inferred,
}

#[derive(Debug, Display, Clone)]
#[display(fmt = "{ty} {ident}")]
pub struct TypedIdent {
	pub ty: Type,
	pub ident: Ident,
}

impl TypedIdent {
	pub fn ident_str(&self) -> String {
		self.ident.to_string()
	}
}

#[derive(Debug, Clone, Default)]
pub struct FuncAttribs {
	pub is_pure: bool,
}

#[derive(Debug, Clone)]
pub struct Func {
	pub return_ty: Type,
	pub args: Vec<TypedIdent>, // TODO: perhaps we might need to change this
	pub body: Scope,
	pub attribs: FuncAttribs,
}

#[derive(Debug, Clone)]
pub enum Expr {
	CharLiteral(String),
	StringLiteral(String),
	NumberLiteral(NumberLiteral),
	Identifier(Ident),
	BinaryOp(Box<Expr>, Operator, Box<Expr>),
	UnaryOp(Operator, Box<Expr>),
	Lambda(Func),
	Call(Box<Expr>, Vec<Expr>),
	Error,
}

#[derive(Debug, Display, Clone)]
pub enum Stmt {
	#[display(fmt = "{_0} = {_1}")]
	Create(TypedIdent, Expr),
	#[display(fmt = "{_0} = {_1}")]
	Set(Ident, Expr),
	#[display(fmt = "{_0} = {_1}")]
	Func(Ident, Func),
	#[display(fmt = "return {_0}")]
	Return(Expr),
	BareExpr(Expr),
}

#[derive(Debug, Clone)]
pub struct Scope {
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
			Type::BareType(x) => f.write_fmt(format_args!("{x}")),
			Type::Builtin(x) => f.write_fmt(format_args!("{x}")),
			Type::Array(x, len) => f.write_fmt(format_args!(
				"{x}{}",
				if let Some(len) = len {
					format!("[{len}]")
				} else {
					"[]".to_string()
				}
			)),
			Type::Ref(x) => f.write_fmt(format_args!("{x}&")),
			Type::Optional(x) => f.write_fmt(format_args!("{x}?")),
			Type::Inferred => f.write_str("~"),
		}
	}
}

impl fmt::Display for FuncAttribs {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		f.write_fmt(format_args!("{}", if self.is_pure { "pure " } else { "" }))
	}
}

impl fmt::Display for Func {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		f.write_fmt(format_args!(
			"{}func ({}) -> {}: {{{}}}",
			self.attribs,
			join_comma(&self.args).unwrap_or("".into()),
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
			Expr::CharLiteral(x) => f.write_fmt(format_args!("{x}")),
			Expr::StringLiteral(x) => f.write_fmt(format_args!("{x}")),
			Expr::NumberLiteral(x) => f.write_fmt(format_args!("{x}")),
			Expr::Identifier(x) => f.write_fmt(format_args!("{x}")),
			Expr::BinaryOp(lhs, op, rhs) => f.write_fmt(format_args!("({lhs} {op} {rhs})")),
			Expr::UnaryOp(op, expr) => f.write_fmt(format_args!("({op}{expr})")),
			Expr::Lambda(func) => f.write_fmt(format_args!("lambda {func}")),
			Expr::Call(callee, args) => f.write_fmt(format_args!(
				"{callee}({})",
				join_comma(args).unwrap_or("".to_string())
			)),
			Expr::Error => f.write_str("[ERROR]"),
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
