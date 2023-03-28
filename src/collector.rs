//! The collector puts the types the inference figured out in variables and
//! fails if some are still unknown.

use crate::{
	common::{Expr, Func, Scope, ScopeFmt, Stmt, Type, TypeSignature},
	hoister::HoistedScope,
	infer::{InferEngine, InferTypeId, InferTypeInfo},
	span::Span,
};
use derive_more::Display;
use std::collections::HashMap;

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

fn collect_inner(
	scope: HoistedScope,
	engine: &InferEngine,
	idents: HashMap<String, InferTypeId>,
) -> CollectedScope {
	let collected = CollectedScope {
		stmts: vec![],
		data: CollectedScopeData::default(),
	};

	let data = scope.data.borrow();
	for ((i, (name, span)), (_, mutable)) in data
		.var_spans
		.iter()
		.enumerate()
		.zip(data.var_mutabilities.iter())
	{
		let engine_ty = engine.tys[&i].clone().follow_ref(engine);
		if matches!(engine_ty, InferTypeInfo::Unknown) {
			todo!("make a good error message");
		}
		todo!("turn infer type info to actual type")
	}

	collected
}

pub fn collect(
	scope: HoistedScope,
	engine: &InferEngine,
	idents: HashMap<String, InferTypeId>,
) -> CollectedScope {
	collect_inner(scope, engine, idents)
}
