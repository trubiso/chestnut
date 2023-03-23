use crate::{
	lexer::{NumberLiteral, NumberLiteralKind, Operator},
	parser::types::Ident,
	span::Span,
};
use derive_more::Display;
use std::{fmt, marker::PhantomData};

// TODO: make GetSpan trait and perhaps change its return type to a ref

pub trait Scope<Expr: fmt::Display> {
	fn braced(&self) -> String
	where
		Self: fmt::Display,
		Self: Sized,
	{
		let body = format!("{}", self);
		format!(
			"{{{}}}",
			if self.stmts().is_empty() {
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
	fn stmts(&self) -> &Vec<Stmt<Expr, Func<Expr, Self>, Self>>
	where
		Self: Sized;
}

pub trait ScopeFmt {
	fn scope_fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result;
}

impl<S: Scope<Expr<S>> + Clone + fmt::Display> ScopeFmt for S {
	fn scope_fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		for stmt in self.stmts() {
			f.write_fmt(format_args!("{stmt}\n"))?;
		}
		Ok(())
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UnscopedExpr {
	CharLiteral(Span, String),
	StringLiteral(Span, String),
	NumberLiteral(Span, NumberLiteral),
	Identifier(Span, Ident),
	BinaryOp(Span, Box<UnscopedExpr>, Operator, Box<UnscopedExpr>),
	UnaryOp(Span, Operator, Box<UnscopedExpr>),
	Call(
		Span,
		Box<UnscopedExpr>,
		Option<Vec<Type>>,
		Vec<UnscopedExpr>,
	),
	Dot(Span, Box<UnscopedExpr>, Box<UnscopedExpr>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr<Sc: Scope<Self> + Clone + fmt::Display> {
	CharLiteral(Span, String),
	StringLiteral(Span, String),
	NumberLiteral(Span, NumberLiteral),
	Identifier(Span, Ident),
	BinaryOp(Span, Box<Expr<Sc>>, Operator, Box<Expr<Sc>>),
	UnaryOp(Span, Operator, Box<Expr<Sc>>),
	Call(Span, Box<Expr<Sc>>, Option<Vec<Type>>, Vec<Expr<Sc>>),
	Dot(Span, Box<Expr<Sc>>, Box<Expr<Sc>>),
	Lambda(Span, Func<Self, Sc>),
}

impl UnscopedExpr {
	pub fn scoped<Sc: Scope<Expr<Sc>> + Clone + fmt::Display>(self) -> Expr<Sc> {
		match self {
			Self::CharLiteral(a, b) => Expr::CharLiteral(a, b),
			Self::StringLiteral(a, b) => Expr::StringLiteral(a, b),
			Self::NumberLiteral(a, b) => Expr::NumberLiteral(a, b),
			Self::Identifier(a, b) => Expr::Identifier(a, b),
			Self::BinaryOp(a, b, c, d) => Expr::BinaryOp(a, b.scoped_box(), c, d.scoped_box()),
			Self::UnaryOp(a, b, c) => Expr::UnaryOp(a, b, c.scoped_box()),
			Self::Call(a, b, c, d) => Expr::Call(
				a,
				b.scoped_box(),
				c,
				d.iter().map(|x| x.clone().scoped()).collect(),
			),
			Self::Dot(a, b, c) => Expr::Dot(a, b.scoped_box(), c.scoped_box()),
		}
	}

	pub fn scoped_box<Sc: Scope<Expr<Sc>> + Clone + fmt::Display>(self) -> Box<Expr<Sc>> {
		Box::new(self.scoped())
	}
}

impl<Sc: Scope<Expr<Sc>> + Clone + fmt::Display> Expr<Sc> {
	pub fn unscoped(self) -> Option<UnscopedExpr> {
		if matches!(self, Self::Lambda(..)) {
			return None;
		}
		Some(match self {
			Self::CharLiteral(a, b) => UnscopedExpr::CharLiteral(a, b),
			Self::StringLiteral(a, b) => UnscopedExpr::StringLiteral(a, b),
			Self::NumberLiteral(a, b) => UnscopedExpr::NumberLiteral(a, b),
			Self::Identifier(a, b) => UnscopedExpr::Identifier(a, b),
			Self::BinaryOp(a, b, c, d) => {
				UnscopedExpr::BinaryOp(a, b.unscoped_box()?, c, d.unscoped_box()?)
			}
			Self::UnaryOp(a, b, c) => UnscopedExpr::UnaryOp(a, b, c.unscoped_box()?),
			Self::Call(a, b, c, d) => UnscopedExpr::Call(
				a,
				b.unscoped_box()?,
				c,
				// TODO: make better error handling
				d.iter().map(|x| x.clone().unscoped().unwrap()).collect(),
			),
			Self::Dot(a, b, c) => UnscopedExpr::Dot(a, b.unscoped_box()?, c.unscoped_box()?),
			_ => unreachable!(),
		})
	}

	pub fn unscoped_box(self) -> Option<Box<UnscopedExpr>> {
		self.unscoped().map(|x| Box::new(x))
	}
}

impl fmt::Display for UnscopedExpr {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Self::CharLiteral(_, x) => f.write_fmt(format_args!("{x}")),
			Self::StringLiteral(_, x) => f.write_fmt(format_args!("{x}")),
			Self::NumberLiteral(_, x) => f.write_fmt(format_args!("{x}")),
			Self::Identifier(_, x) => f.write_fmt(format_args!("{x}")),
			Self::BinaryOp(_, lhs, op, rhs) => f.write_fmt(format_args!("({lhs} {op} {rhs})")),
			Self::UnaryOp(_, op, expr) => f.write_fmt(format_args!("({op}{expr})")),
			Self::Call(_, callee, generics, args) => f.write_fmt(format_args!(
				"{callee}{}({})",
				generics
					.as_ref()
					.map(|g| join_comma(g)
						.map(|x| format!("<{x}>"))
						.unwrap_or("".to_string()))
					.unwrap_or("".to_string()),
				join_comma(args).unwrap_or("".to_string())
			)),
			Self::Dot(_, lhs, rhs) => f.write_fmt(format_args!("{lhs}.{rhs}")),
		}
	}
}

impl<Sc: Scope<Self> + Clone + fmt::Display> fmt::Display for Expr<Sc> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Self::Lambda(_, func) => f.write_fmt(format_args!("lambda {func}")),
			other => other.clone().unscoped().unwrap().fmt(f),
		}
	}
}

impl UnscopedExpr {
	pub fn span(&self) -> Span {
		match self {
			Self::CharLiteral(x, ..)
			| Self::StringLiteral(x, ..)
			| Self::NumberLiteral(x, ..)
			| Self::Identifier(x, ..)
			| Self::UnaryOp(x, ..)
			| Self::Call(x, ..)
			| Self::Dot(x, ..) => x.clone(),
			Self::BinaryOp(x, a, _, b) => a.span() + x.clone() + b.span(),
		}
	}
}

impl<S: Scope<Self> + Clone + fmt::Display> Expr<S> {
	pub fn span(&self) -> Span {
		match self {
			Self::Lambda(x, ..) => x.clone(),
			other => other.clone().unscoped().unwrap().span(),
		}
	}
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct FuncAttribs {
	pub is_pure: bool,
	pub is_mut: bool,
	pub is_unsafe: bool,
}

impl fmt::Display for FuncAttribs {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		f.write_fmt(format_args!(
			"{}{}{}",
			if self.is_pure { "pure " } else { "" },
			if self.is_mut { "mut " } else { "" },
			if self.is_unsafe { "unsafe " } else { "" },
		))
	}
}

#[derive(Debug, Clone)]
pub struct Func<Expr: fmt::Display, Sc: Scope<Expr>> {
	pub span: Span,
	pub return_ty: Type,
	pub args: Vec<TypedIdent>, // TODO: perhaps we might need to change this
	pub generics: Vec<Ident>,  // TODO: same here
	pub body: Sc,
	pub attribs: FuncAttribs,
	pub decl_span: Span,
	// TODO: unneeded! this is to avoid infinitely recursive types
	pub _expr: PhantomData<Expr>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FuncSignature {
	pub generics: Vec<Ident>,
	pub arg_tys: Vec<Type>,
	pub return_ty: Type,
}

impl<E: fmt::Display + Clone, S: Scope<E>> Func<E, S> {
	pub fn signature(&self) -> FuncSignature {
		FuncSignature {
			generics: self.generics.clone(),
			arg_tys: self.args.iter().map(|x| x.ty.clone()).collect(),
			return_ty: self.return_ty.clone(),
		}
	}
}

impl<E: fmt::Display + Clone + PartialEq, S: Scope<E>> PartialEq for Func<E, S> {
	fn eq(&self, other: &Self) -> bool {
		self.signature() == other.signature()
	}
}

impl<E: fmt::Display + Clone + PartialEq, S: Scope<E>> Eq for Func<E, S> {}

impl<E: fmt::Display, S: Scope<E> + fmt::Display> fmt::Display for Func<E, S> {
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Stmt<Expr: fmt::Display, Fn, Sc: Scope<Expr>> {
	Create(Span, Privacy, TypedIdent, bool, Expr),
	Declare(Span, Privacy, TypedIdent, bool),
	Set(Span, Ident, Expr),
	Func(Span, Privacy, Ident, Fn),
	Return(Span, Expr),
	Class(
		Span,
		Privacy,
		Ident,
		Vec<Ident>, /* generics */
		Span,
		Sc,
	),
	Import(Span, bool, Ident),
	BareExpr(Span, Expr),
	Unsafe(Span, Sc),
	Cpp(Span, String),
}

impl<E: fmt::Display, F, S: Scope<E>> Stmt<E, F, S> {
	pub fn span(&self) -> Span {
		match self {
			Self::Create(x, ..)
			| Self::Declare(x, ..)
			| Self::Set(x, ..)
			| Self::Func(x, ..)
			| Self::Return(x, ..)
			| Self::Class(x, ..)
			| Self::Import(x, ..)
			| Self::BareExpr(x, ..)
			| Self::Unsafe(x, ..)
			| Self::Cpp(x, ..) => x.clone(),
		}
	}

	pub fn variant(&self) -> String {
		match self {
			Self::Create(..) => "creation".into(),
			Self::Declare(..) => "declaration".into(),
			Self::Set(..) => "set".into(),
			Self::Func(..) => "function".into(),
			Self::Return(..) => "return".into(),
			Self::Class(..) => "class".into(),
			Self::Import(..) => "import".into(),
			Self::BareExpr(..) => "bare expression".into(),
			Self::Unsafe(..) => "unsafe scope".into(),
			Self::Cpp(..) => "inline c++".into(),
		}
	}
}

impl<E: fmt::Display, F: fmt::Display, S: Scope<E> + fmt::Display> fmt::Display for Stmt<E, F, S> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Stmt::Create(_span, privacy, ty_ident, is_mut, expr) => f.write_fmt(format_args!(
				"{privacy}{}{ty_ident} = {expr};",
				if *is_mut { "mut " } else { "" }
			)),
			Stmt::Declare(_span, privacy, ty_ident, is_mut) => f.write_fmt(format_args!(
				"declare {privacy}{}{ty_ident};",
				if *is_mut { "mut " } else { "" }
			)),
			Stmt::Set(_span, ident, expr) => f.write_fmt(format_args!("{ident} = {expr};")),
			Stmt::Func(_span, privacy, ident, func) => {
				f.write_fmt(format_args!("{privacy}{ident}{func}"))
			}
			Stmt::Return(_span, expr) => f.write_fmt(format_args!("return {expr};")),
			Stmt::Class(_span, privacy, ident, generics, _decl_span, body) => {
				f.write_fmt(format_args!(
					"class {privacy}{ident}{} {}",
					join_generics(generics),
					body.braced()
				))
			}
			Stmt::Import(_span, glob, imported) => f.write_fmt(format_args!(
				"import {imported}{};",
				if *glob { "::*" } else { "" }
			)),
			Stmt::BareExpr(_span, expr) => f.write_fmt(format_args!("{expr};")),
			Stmt::Unsafe(_span, scope) => f.write_fmt(format_args!("unsafe {}", scope.braced())),
			Stmt::Cpp(_span, code) => f.write_fmt(format_args!("cpp {code}")),
		}
	}
}

#[derive(Debug, Display, Clone, PartialEq, Eq)]
pub enum BuiltinType {
	// TODO: func signature type
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

impl BuiltinType {
	pub fn from_name(name: &str) -> Option<Self> {
		match name {
			"i8" => Some(Self::I8),
			"i16" => Some(Self::I16),
			"i32" => Some(Self::I32),
			"i64" => Some(Self::I64),
			"i128" => Some(Self::I128),
			"iz" => Some(Self::IZ),
			"u8" => Some(Self::U8),
			"u16" => Some(Self::U16),
			"u32" => Some(Self::U32),
			"u64" => Some(Self::U64),
			"u128" => Some(Self::U128),
			"uz" => Some(Self::UZ),
			"f16" => Some(Self::F16),
			"f32" => Some(Self::F32),
			"f64" => Some(Self::F64),
			"f128" => Some(Self::F128),
			"void" => Some(Self::Void),
			"bool" => Some(Self::Bool),
			"string" => Some(Self::String),
			"char" => Some(Self::Char),
			_ => None,
		}
	}
}

pub enum NumberLiteralKindKind {
	None,
	Signed,
	Unsigned,
	Float,
}

impl NumberLiteralKind {
	pub fn as_ty(&self) -> BuiltinType {
		match self {
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

	pub fn as_useful_infer_info(&self) -> (Option<u8>, NumberLiteralKindKind) {
		match self {
			NumberLiteralKind::I8 => (Some(8), NumberLiteralKindKind::Signed),
			NumberLiteralKind::I16 => (Some(16), NumberLiteralKindKind::Signed),
			NumberLiteralKind::I32 => (Some(32), NumberLiteralKindKind::Signed),
			NumberLiteralKind::I64 => (Some(64), NumberLiteralKindKind::Signed),
			NumberLiteralKind::I128 => (Some(128), NumberLiteralKindKind::Signed),
			NumberLiteralKind::IZ => (None, NumberLiteralKindKind::Signed),
			NumberLiteralKind::U8 => (Some(8), NumberLiteralKindKind::Unsigned),
			NumberLiteralKind::U16 => (Some(16), NumberLiteralKindKind::Unsigned),
			NumberLiteralKind::U32 => (Some(32), NumberLiteralKindKind::Unsigned),
			NumberLiteralKind::U64 => (Some(64), NumberLiteralKindKind::Unsigned),
			NumberLiteralKind::U128 => (Some(128), NumberLiteralKindKind::Unsigned),
			NumberLiteralKind::UZ => (None, NumberLiteralKindKind::Unsigned),
			NumberLiteralKind::F16 => (Some(16), NumberLiteralKindKind::Float),
			NumberLiteralKind::F32 => (Some(32), NumberLiteralKindKind::Float),
			NumberLiteralKind::F64 => (Some(64), NumberLiteralKindKind::Float),
			NumberLiteralKind::F128 => (Some(128), NumberLiteralKindKind::Float),

			NumberLiteralKind::U => (Some(0), NumberLiteralKindKind::Unsigned),
			NumberLiteralKind::F => (Some(0), NumberLiteralKindKind::Float),
			NumberLiteralKind::None => (Some(0), NumberLiteralKindKind::None),
		}
	}
}

impl NumberLiteral {
	pub fn as_ty(&self) -> BuiltinType {
		self.kind.as_ty()
	}
}

// TODO: remove BareType
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BareType {
	pub ident: Ident,        // typename
	pub generics: Vec<Type>, // generics
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

#[derive(Debug, Clone)]
pub enum Type {
	BareType(Span, BareType),
	Builtin(Span, BuiltinType),
	Array(Span, Box<Self>, Option<Box<UnscopedExpr>>),
	Ref(Span, Box<Self>, bool),
	Optional(Span, Box<Self>),
	Function(Span, Box<FuncSignature>),
	Inferred(Span),
}

impl PartialEq for Type {
	fn eq(&self, other: &Self) -> bool {
		match self {
			Self::BareType(_, x) => {
				if let Self::BareType(_, y) = other {
					x == y
				} else {
					false
				}
			}
			Self::Builtin(_, x) => {
				if let Self::Builtin(_, y) = other {
					x == y
				} else {
					false
				}
			}
			Self::Array(_, a, b) => {
				if let Self::Array(_, c, d) = other {
					a == c && b == d
				} else {
					false
				}
			}
			Self::Ref(_, x, a) => {
				if let Self::Ref(_, y, b) = other {
					*x == *y && *a == *b
				} else {
					false
				}
			}
			Self::Optional(_, x) => {
				if let Self::Optional(_, y) = other {
					*x == *y
				} else {
					false
				}
			}
			Self::Function(_, x) => {
				if let Self::Function(_, y) = other {
					*x == *y
				} else {
					false
				}
			}
			Self::Inferred(_) => matches!(other, Self::Inferred(_)),
		}
	}
}

impl Eq for Type {}

impl Type {
	pub fn span(&self) -> Span {
		match self {
			Self::BareType(x, ..)
			| Self::Builtin(x, ..)
			| Self::Array(x, ..)
			| Self::Ref(x, ..)
			| Self::Optional(x, ..)
			| Self::Function(x, ..)
			| Self::Inferred(x, ..) => x.clone(),
		}
	}

	pub fn is_void(&self) -> bool {
		let Self::Builtin(_, x) = self else { return false; };
		*x == BuiltinType::Void
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
			Type::Ref(_, x, m) => f.write_fmt(format_args!("{x}{}&", if *m { "mut" } else { "" })),
			Type::Optional(_, x) => f.write_fmt(format_args!("{x}?")),
			// TODO: implement fmt::Display for FuncSignature
			Type::Function(_, _x) => f.write_str("see above"),
			Type::Inferred(_) => f.write_str("~"),
		}
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

pub fn join_comma<T: fmt::Display>(vec: &[T]) -> Option<String> {
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
