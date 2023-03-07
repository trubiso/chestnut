use crate::lexer::{NumberLiteral, NumberLiteralKind, Operator, Token};
use crate::span::Span;
use chumsky::prelude::*;
use derive_more::Display;
use std::fmt;

pub trait TokenParser<T> = Parser<Token, T, Error = Simple<Token, Span>>;
pub type TokenRecursive<'a, T> = Recursive<'a, Token, T, Simple<Token, Span>>;
pub type ScopeRecursive<'a> = TokenRecursive<'a, Scope>;
pub type ExprRecursive<'a> = TokenRecursive<'a, Expr>;

#[derive(Debug, Display, Clone, PartialEq, Eq)]
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BareType {
	pub ident: Ident,           // typename
	pub generics: Vec<Generic>, // generics
}

#[derive(Debug, Display, Clone, PartialEq, Eq)]
pub enum BuiltinType {
	#[display(fmt = "i8")]
	I8,
	#[display(fmt = "i16")]
	I16,
	#[display(fmt = "i32")]
	I32,
	#[display(fmt = "i64")]
	I64,
	#[display(fmt = "i128")]
	I128,
	#[display(fmt = "iz")]
	IZ,
	#[display(fmt = "u8")]
	U8,
	#[display(fmt = "u16")]
	U16,
	#[display(fmt = "u32")]
	U32,
	#[display(fmt = "u64")]
	U64,
	#[display(fmt = "u128")]
	U128,
	#[display(fmt = "uz")]
	UZ,
	#[display(fmt = "f16")]
	F16,
	#[display(fmt = "f32")]
	F32,
	#[display(fmt = "f64")]
	F64,
	#[display(fmt = "f128")]
	F128,
	#[display(fmt = "void")]
	Void,
	#[display(fmt = "bool")]
	Bool,
	#[display(fmt = "string")]
	String,
	#[display(fmt = "char")]
	Char,
	#[display(fmt = "error")]
	Error,
}

impl NumberLiteral {
	pub fn as_ty(&self) -> BuiltinType {
		match self.kind {
			NumberLiteralKind::I8 => BuiltinType::I8,
			NumberLiteralKind::I16 => BuiltinType::I16,
			NumberLiteralKind::I32 => BuiltinType::I32,
			NumberLiteralKind::I64 => BuiltinType::I64,
			NumberLiteralKind::I128 => BuiltinType::I128,
			NumberLiteralKind::IZ => BuiltinType::IZ,
			NumberLiteralKind::U8 => BuiltinType::U8,
			NumberLiteralKind::U16 => BuiltinType::U16,
			NumberLiteralKind::U32 => BuiltinType::U32,
			NumberLiteralKind::U64 => BuiltinType::U64,
			NumberLiteralKind::U128 => BuiltinType::U128,
			NumberLiteralKind::UZ => BuiltinType::UZ,
			NumberLiteralKind::F16 => BuiltinType::F16,
			NumberLiteralKind::F32 => BuiltinType::F32,
			NumberLiteralKind::F64 => BuiltinType::F64,
			NumberLiteralKind::F128 => BuiltinType::F128,

			NumberLiteralKind::U => BuiltinType::U32,
			NumberLiteralKind::F => BuiltinType::F32,
			NumberLiteralKind::None => BuiltinType::I32,
		}
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
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

#[derive(Debug, Display, Clone, PartialEq, Eq)]
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
	Private(Span),
	#[display(fmt = "protected ")]
	Protected(Span),
	#[display(fmt = "public ")]
	Public(Span),
	#[display(fmt = "export ")]
	Export(Span),
	#[display(fmt = "")]
	Default,
}

impl Privacy {
	pub fn span(&self) -> Option<Span> {
		match self {
			Privacy::Private(x)
			| Privacy::Protected(x)
			| Privacy::Public(x)
			| Privacy::Export(x) => Some(x.clone()),
			Privacy::Default => None,
		}
	}

	pub fn is_private(&self) -> bool {
		matches!(self, Privacy::Private(_))
	}
	pub fn is_protected(&self) -> bool {
		matches!(self, Privacy::Protected(_))
	}
	pub fn is_public(&self) -> bool {
		matches!(self, Privacy::Public(_))
	}
	pub fn is_export(&self) -> bool {
		matches!(self, Privacy::Export(_))
	}
	pub fn is_default(&self) -> bool {
		*self == Privacy::Default
	}
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct FuncAttribs {
	pub is_pure: bool,
	pub is_mut: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Func {
	pub span: Span,
	pub return_ty: Type,
	pub args: Vec<TypedIdent>, // TODO: perhaps we might need to change this
	pub generics: Vec<Ident>,  // TODO: same here
	pub body: Scope,
	pub attribs: FuncAttribs,
}

#[derive(Debug, Clone, PartialEq, Eq)]
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Stmt {
	Create(Span, Privacy, TypedIdent, Expr),
	Declare(Span, Privacy, TypedIdent),
	Set(Span, Ident, Expr),
	Func(Span, Privacy, Ident, Func),
	Return(Span, Expr),
	Class(Span, Privacy, Ident, Vec<Ident> /* generics */, Scope),
	BareExpr(Span, Expr),
}

impl fmt::Display for Stmt {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Stmt::Create(_span, privacy, ty_ident, expr) => {
				f.write_fmt(format_args!("{privacy}{ty_ident} = {expr};"))
			}
			Stmt::Declare(_span, privacy, ty_ident) => {
				f.write_fmt(format_args!("declare {privacy}{ty_ident};"))
			}
			Stmt::Set(_span, ident, expr) => f.write_fmt(format_args!("{ident} = {expr};")),
			Stmt::Func(_span, privacy, ident, func) => {
				f.write_fmt(format_args!("{privacy}{ident}{func}"))
			}
			Stmt::Return(_span, expr) => f.write_fmt(format_args!("return {expr};")),
			Stmt::Class(_span, privacy, ident, generics, body) => f.write_fmt(format_args!(
				"class {privacy}{ident}{} {}",
				join_generics(generics),
				body.braced()
			)),
			Stmt::BareExpr(_span, expr) => f.write_fmt(format_args!("{expr};")),
		}
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Scope {
	pub span: Span,
	pub stmts: Vec<Stmt>,
}

impl Scope {
	pub fn braced(&self) -> String {
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
			"{}({}) {}-> {} {}",
			join_generics(&self.generics),
			join_comma(&self.args).unwrap_or("".into()),
			self.attribs,
			self.return_ty,
			self.body.braced(),
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

fn join_generics(generics: &[Ident]) -> String {
	match join_comma(generics) {
		Some(x) => "<".to_string() + &x + ">",
		None => "".into(),
	}
}
