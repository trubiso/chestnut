//! The collector puts the types the inference figured out in variables and
//! fails if some are still unknown. It also checks for use before define.

use crate::{
	common::{BareType, BuiltinType, Expr, Func, Scope, ScopeFmt, Stmt, Type, TypeSignature},
	hoister::HoistedScope,
	ident,
	infer::{InferEngine, InferTypeId, InferTypeInfo},
	span::Span,
};
use codespan_reporting::diagnostic::{Diagnostic, Label};
use lazy_static::lazy_static;
use std::{collections::HashMap, sync::Mutex};

lazy_static! {
	static ref DIAGNOSTICS: Mutex<Vec<Diagnostic<usize>>> = Mutex::new(vec![]);
}

pub fn add_diagnostic(diagnostic: Diagnostic<usize>) {
	DIAGNOSTICS.lock().unwrap().push(diagnostic);
}

pub type CollectedFunc = Func<CollectedExpr, CollectedScope>;
pub type CollectedExpr = Expr<CollectedScope>;
pub type CollectedStmt = Stmt<CollectedExpr, CollectedFunc, CollectedScope>;

#[derive(Clone)]
pub struct CollectedVar {
	pub name: String,
	pub ty: Type,
	pub mutable: bool,
	pub value: Option<Option<CollectedExpr>>,
}

#[derive(Clone)]
pub struct CollectedMadeType {
	pub name: String,
	pub signature: TypeSignature,
	pub body: Option<CollectedScope>,
}

#[derive(Clone, Default)]
pub struct CollectedScopeData {
	pub vars: HashMap<String, CollectedVar>,
	pub funcs: HashMap<String, CollectedFunc>,
	pub types: HashMap<String, CollectedMadeType>,
	pub var_spans: HashMap<String, Span>,
	pub func_spans: HashMap<String, Span>,
	pub type_spans: HashMap<String, Span>,
}

#[derive(Clone)]
pub struct CollectedScope {
	pub stmts: Vec<CollectedStmt>,
	pub data: CollectedScopeData,
}

impl Scope<CollectedExpr> for CollectedScope {
	fn stmts(&self) -> &Vec<Stmt<CollectedExpr, Func<CollectedExpr, Self>, Self>>
	where
		Self: Sized,
	{
		&self.stmts
	}
}

impl std::fmt::Display for CollectedScope {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		self.scope_fmt(f)
	}
}

impl InferTypeInfo {
	pub fn is_partially_unknown(&self, engine: &InferEngine) -> bool {
		let duct_tape = |x| {
			engine
				.tys
				.get(&x)
				.map(|x| x.is_partially_unknown(engine))
				.unwrap_or(true)
		};
		let helper = |x: &usize| duct_tape(*x);
		let helper_arr = |x: &[usize]| {
			x.iter()
				.copied()
				.map(duct_tape)
				.reduce(|acc, b| acc || b)
				.unwrap_or(false)
		};
		match self {
			Self::Unknown => true,
			Self::SameAs(x) => helper(x),
			Self::Generics(x, g) => helper(x) || helper_arr(g),
			Self::TypeSignature(..) => false,
			Self::FuncSignature(_, g, a, r) => helper_arr(g) || helper_arr(a) || helper(r),
			Self::AnySigned | Self::AnyUnsigned | Self::AnyFloat => false,
			Self::KnownNumber(..)
			| Self::KnownVoid
			| Self::KnownBool
			| Self::KnownString
			| Self::KnownChar => false,
			Self::Ref(x) => helper(x),
			Self::Generic(_) => false,
			Self::UnknownGeneric(_) => true,
			Self::Bottom => false,
		}
	}

	pub fn collect(&self, engine: &InferEngine, span: Span) -> Type {
		// we won't go to codegen if this failed -> it's ok to put any junk type here
		let error_ty = Type::Inferred(span.clone());
		let duct_tape = |x| {
			engine
				.tys
				.get(&x)
				.map(|x| x.collect(engine, span.clone()))
				.unwrap_or_else(|| error_ty.clone())
		};
		let helper = |x: &usize| duct_tape(*x);
		let helper_arr = |x: &[usize]| x.iter().copied().map(duct_tape).collect();
		match self {
			Self::Unknown => error_ty,
			Self::SameAs(x) => helper(x),
			Self::Generics(x, g) => Type::BareType(
				span.clone(),
				BareType {
					ident: ident!(
						span.clone(),
						match &engine.tys[x] {
							Self::TypeSignature(s, _) => s,
							_ => unreachable!(),
						}
					),
					generics: helper_arr(g),
				},
			),
			Self::TypeSignature(..) => unreachable!(),
			Self::FuncSignature(..) => todo!(),
			Self::AnySigned => Type::Builtin(span, BuiltinType::I32),
			Self::AnyUnsigned => Type::Builtin(span, BuiltinType::U32),
			Self::AnyFloat => Type::Builtin(span, BuiltinType::F32),
			Self::KnownNumber(x) => Type::Builtin(span, x.as_ty()),
			Self::KnownVoid => Type::Builtin(span, BuiltinType::Void),
			Self::KnownBool => Type::Builtin(span, BuiltinType::Bool),
			Self::KnownString => Type::Builtin(span, BuiltinType::String),
			Self::KnownChar => Type::Builtin(span, BuiltinType::Char),
			// TODO: mutability in type inference
			Self::Ref(x) => Type::Ref(span.clone(), Box::new(helper(x)), false),
			Self::Generic(x) => Type::BareType(
				span.clone(),
				BareType {
					ident: ident!(span, x.clone()),
					generics: vec![],
				},
			),
			Self::UnknownGeneric(_) => error_ty,
			Self::Bottom => error_ty,
		}
	}
}

fn collect_inner(
	scope: &HoistedScope,
	engine: &InferEngine,
	idents: &HashMap<String, InferTypeId>,
) -> CollectedScope {
	let mut collected = CollectedScope {
		stmts: vec![],
		data: CollectedScopeData::default(),
	};

	let data = scope.data.borrow();
	for ((name, span), (_, mutable)) in data.var_spans.iter().zip(data.var_mutabilities.iter()) {
		let engine_ty = match idents.get(name) {
			None => todo!("ident {name} not found"),
			Some(x) => match engine.tys.get(x) {
				None => todo!("ident {name} has no type (???)"),
				Some(x) => x.clone().follow_ref(engine),
			},
		};
		if engine_ty.is_partially_unknown(engine) {
			add_diagnostic(
				Diagnostic::error()
					.with_message("explicit type required")
					.with_labels(vec![Label::primary(span.file_id, span.range())
						.with_message("could not infer type of variable")]),
			);
		}
		let ty = engine_ty.collect(engine, span.clone());
		collected.data.vars.insert(name.clone(), CollectedVar { name: name.clone(), ty, mutable: *mutable, value: None });
		//todo!("turn infer type info to actual type")
	}

	for stmt in scope.stmts.borrow().iter() {
		match stmt {
			Stmt::Func(_, _, _, f) => {
				collect_inner(&f.body, engine, idents);
			}
			/*Stmt::Class(_, _, _, _, _, b) => {
				collect_inner(b, engine, idents);
			}*/
			Stmt::Unsafe(_, b) => {
				collect_inner(b, engine, idents);
			}
			_ => {}
		}
	}

	collected
}

pub fn collect(
	scope: HoistedScope,
	engine: &InferEngine,
	idents: &HashMap<String, InferTypeId>,
) -> (CollectedScope, Vec<Diagnostic<usize>>) {
	(
		collect_inner(&scope, engine, idents),
		DIAGNOSTICS.lock().unwrap().clone(),
	)
}
