use crate::{
	common::{BareType, Expr, Func, FuncSignature, Scope, ScopeFmt, Stmt, Type, TypedIdent},
	get_datum,
	parser::types::{
		ParserExpr, ParserFunc, ParserScope, ParserStmt, ParserType, ParserTypedIdent,
	},
	span::Span,
};
use codespan_reporting::diagnostic::{Diagnostic, Label};
use lazy_static::lazy_static;
use std::{collections::HashMap, fmt, sync::Mutex};

// TODO: we are converting from parsed elements to hoisted elements. but we are
// hoisting them in strange places and i feel like it defeats the entire purpose
// of hoisting. unless, of course, you consider that the only "hoisting" in
// types happens inside of arrays because they can take any expression, which we
// can easily regulate in `resolve.rs`. this code is so messy it probably
// requires a rewrite LOL

lazy_static! {
	static ref DIAGNOSTICS: Mutex<Vec<Diagnostic<usize>>> = Mutex::new(vec![]);
}

pub fn add_diagnostic(diagnostic: Diagnostic<usize>) {
	DIAGNOSTICS.lock().unwrap().push(diagnostic);
}

#[derive(Debug, Default, Clone)]
pub struct MadeTypeSignature {
	pub fields: HashMap<String, HoistedType>,
	pub funcs: HashMap<String, HoistedFuncSignature>,
}

pub type HoistedType = Type<HoistedExpr>;
pub type HoistedTypedIdent = TypedIdent<HoistedType>;
pub type HoistedBareType = BareType<HoistedType>;
pub type HoistedFunc = Func<HoistedExpr, HoistedScope>;
pub type HoistedFuncSignature = FuncSignature<HoistedType>;
pub type HoistedExpr = Expr<HoistedScope>;
pub type HoistedStmt = Stmt<HoistedExpr, HoistedFunc, HoistedScope>;

#[derive(Debug, Default, Clone)]
pub struct HoistedScopeData {
	pub vars: HashMap<String, HoistedType>,
	pub var_spans: HashMap<String, Span>,
	pub funcs: HashMap<String, HoistedFuncSignature>,
	pub func_spans: HashMap<String, Span>,
	pub types: HashMap<String, MadeTypeSignature>,
	pub type_spans: HashMap<String, Span>,
	pub stmts: Vec<HoistedStmt>,
}

impl std::ops::Add for HoistedScopeData {
	type Output = HoistedScopeData;

	fn add(self, rhs: Self) -> Self::Output {
		Self {
			vars: rhs.vars.into_iter().chain(self.vars).collect(),
			var_spans: rhs.var_spans.into_iter().chain(self.var_spans).collect(),
			funcs: rhs.funcs.into_iter().chain(self.funcs).collect(),
			func_spans: rhs.func_spans.into_iter().chain(self.func_spans).collect(),
			types: rhs.types.into_iter().chain(self.types).collect(),
			type_spans: rhs.type_spans.into_iter().chain(self.type_spans).collect(),
			stmts: rhs.stmts.into_iter().chain(self.stmts).collect(),
		}
	}
}

#[derive(Debug, Clone)]
pub struct HoistedScope {
	pub data: HoistedScopeData,
	pub inherit: HoistedScopeData,
	pub span: Span,
}

impl fmt::Display for HoistedScope {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		self.scope_fmt(f)
	}
}

impl HoistedScope {
	pub fn new(span: Span) -> Self {
		Self {
			data: HoistedScopeData::default(),
			inherit: HoistedScopeData::default(),
			span,
		}
	}

	get_datum!(get_type get_type_mut has_type add_type => types (MadeTypeSignature));
	get_datum!(get_type_span get_type_span_mut has_type_span add_type_span => type_spans (Span));
	get_datum!(get_var get_var_mut has_var add_var => vars (HoistedType));
	get_datum!(get_var_span get_var_span_mut has_var_span add_var_span => var_spans (Span));
	get_datum!(get_func get_func_mut has_func add_func => funcs (HoistedFuncSignature));
	get_datum!(get_func_span get_func_span_mut has_func_span add_func_span => func_spans (Span));

	pub fn stmts_mut(&mut self) -> &mut Vec<HoistedStmt> {
		&mut self.data.stmts
	}
}

impl Scope<HoistedExpr> for HoistedScope {
	fn stmts(&self) -> &Vec<Stmt<HoistedExpr, crate::common::Func<HoistedExpr, Self>, Self>>
	where
		Self: Sized,
	{
		&self.data.stmts
	}
}

impl ParserExpr {
	// NOTE: what should we do here? maybe just move the values to the "hoisted"
	// ones, i don't think we need to do anything. we could even get rid of inherit
	// and save like 100% of this code's .clone()s
	pub fn hoist(self, _inherit: Option<HoistedScope>) -> HoistedExpr {
		todo!()
	}
}

impl ParserTypedIdent {
	pub fn hoist(self, inherit: Option<HoistedScope>) -> HoistedTypedIdent {
		HoistedTypedIdent {
			span: self.span,
			ty: self.ty.hoist(inherit),
			ident: self.ident,
		}
	}
}

impl ParserType {
	pub fn hoist(self, inherit: Option<HoistedScope>) -> HoistedType {
		match self {
			Type::BareType(s, t) => Type::BareType(
				s,
				HoistedBareType {
					ident: t.ident,
					generics: t
						.generics
						.iter()
						.map(|x| x.clone().hoist(inherit.clone()))
						.collect(),
				},
			),
			Type::Array(s, l, r) => Type::Array(
				s,
				Box::new(l.hoist(inherit.clone())),
				r.map(|x| Box::new(x.hoist(inherit.clone()))),
			),
			_other => todo!(),
		}
	}
}

impl ParserFunc {
	pub fn hoist(self, inherit: Option<HoistedScope>) -> HoistedFunc {
		HoistedFunc {
			span: self.span,
			return_ty: self.return_ty.hoist(inherit.clone()),
			args: self
				.args
				.iter()
				.map(|x| x.clone().hoist(inherit.clone()))
				.collect(),
			generics: self.generics,
			body: hoist(self.body, inherit)
				.unwrap_or_else(|_| HoistedScope::new(self.decl_span.clone())),
			attribs: self.attribs,
			decl_span: self.decl_span,
		}
	}
}

macro_rules! ignore_hoist {
	($x:expr, $h:expr;$($v:ident($($p:tt),*))*) => {
		match $x {
			$(Stmt::$v($($p),*) => $h.stmts_mut().push(HoistedStmt::$v($($p),*)),)*
			_ => unreachable!()
		}
	};
}

fn redeclaration_error(name: &str, span: &Span, decl_span: &Span) {
	add_diagnostic(
		Diagnostic::error()
			.with_message(format!("{name} redeclaration"))
			.with_labels(vec![
				Label::primary(span.file_id, span.range()).with_message("new declaration here"),
				Label::secondary(decl_span.file_id, decl_span.range())
					.with_message("original declaration here"),
			]),
	)
}

pub fn hoist(
	scope: ParserScope,
	inherit: Option<HoistedScope>,
) -> Result<HoistedScope, Vec<Diagnostic<usize>>> {
	let inherit_hoisted = inherit.unwrap_or_else(|| HoistedScope::new(scope.span.clone()));
	let mut hoisted = HoistedScope::new(scope.span);
	hoisted.inherit = inherit_hoisted.data + inherit_hoisted.inherit;
	// rust is fed up with my types
	let mut for_later: Vec<ParserStmt> = Vec::new();
	for stmt in scope.stmts {
		match stmt {
			Stmt::Create(span, privacy, ty_ident, is_mut, expr) => {
				if hoisted.has_var(&ty_ident.ident_str()) {
					let decl_span = hoisted.get_var_span(&ty_ident.ident_str()).unwrap();
					redeclaration_error("variable", &span, decl_span);
				}
				hoisted.add_var(
					&ty_ident.ident_str(),
					ty_ident.ty.clone().hoist(Some(hoisted.clone())),
				);
				hoisted.add_var_span(&ty_ident.ident_str(), span.clone());
				let hoisted_ty_ident = ty_ident.hoist(Some(hoisted.clone()));
				// NOTE: ???????
				let hoisted_expr = expr.hoist(Some(hoisted.clone()));
				hoisted.stmts_mut().push(HoistedStmt::Create(
					span,
					privacy,
					hoisted_ty_ident,
					is_mut,
					hoisted_expr,
				));
			}
			Stmt::Declare(span, privacy, ty_ident, is_mut) => {
				if hoisted.has_var(&ty_ident.ident_str()) {
					let decl_span = hoisted.get_var_span(&ty_ident.ident_str()).unwrap();
					redeclaration_error("variable", &span, decl_span);
				}
				hoisted.add_var(
					&ty_ident.ident_str(),
					ty_ident.ty.clone().hoist(Some(hoisted.clone())),
				);
				hoisted.add_var_span(&ty_ident.ident_str(), span.clone());
				let hoisted_ty_ident = ty_ident.hoist(Some(hoisted.clone()));
				hoisted.stmts_mut().push(HoistedStmt::Declare(
					span,
					privacy,
					hoisted_ty_ident,
					is_mut,
				));
			}
			Stmt::Set(span, ident, expr) => {
				if !hoisted.has_var(&ident.to_string()) {
					// TODO: add diagnostic (non-existing variable)
				}
				let hoisted_expr = expr.hoist(Some(hoisted.clone()));
				hoisted
					.stmts_mut()
					.push(HoistedStmt::Set(span, ident, hoisted_expr));
			}
			Stmt::Func(span, privacy, ident, func) => {
				if hoisted.has_func(&ident.to_string()) {
					let decl_span = hoisted.get_func_span(&ident.to_string()).unwrap();
					redeclaration_error("function", &func.decl_span, decl_span);
				}
				// TODO: ouch this .clone() is expensive
				hoisted.add_func(&ident.to_string(), func.clone().hoist(None).signature());
				hoisted.add_func_span(&ident.to_string(), span.clone());
				for_later.push(Stmt::Func(span, privacy, ident, func));
			}
			Stmt::Class(span, privacy, ident, generics, decl_span, body) => {
				if hoisted.has_type(&ident.to_string()) {
					let old_decl_span = hoisted.get_type_span(&ident.to_string()).unwrap();
					redeclaration_error("type", &decl_span, old_decl_span);
				}
				// TODO: this is stupid, as it will not hoist fully in the top level
				let hoisted_scope = hoist(body, Some(hoisted.clone()))
					.unwrap_or_else(|_| HoistedScope::new(span.clone()));
				// TODO: get the actual made type signature
				hoisted.add_type(&ident.to_string(), MadeTypeSignature::default());
				hoisted.add_type_span(&ident.to_string(), decl_span.clone());
				// TODO: hoist the type in for_later
				hoisted.stmts_mut().push(HoistedStmt::Class(
					span,
					privacy,
					ident,
					generics,
					decl_span,
					hoisted_scope,
				));
			}
			Stmt::Unsafe(span, scope) => {
				// TODO: properly hoist, like funcs
				let hoisted_scope = hoist(scope, Some(hoisted.clone()))
					.unwrap_or_else(|_| HoistedScope::new(span.clone()));
				hoisted
					.stmts_mut()
					.push(HoistedStmt::Unsafe(span, hoisted_scope));
			}
			// NOTE: do NOT hoist return. why would you return a value _after_ the return keyword???
			Stmt::Return(span, value) => {
				let hoisted_value = value.hoist(Some(hoisted.clone()));
				hoisted
					.stmts_mut()
					.push(HoistedStmt::Return(span, hoisted_value));
			}
			// TODO: properly hoist bare exprs
			Stmt::BareExpr(span, expr) => {
				let hoisted_expr = expr.hoist(Some(hoisted.clone()));
				hoisted
					.stmts_mut()
					.push(HoistedStmt::BareExpr(span, hoisted_expr));
			}
			stmt => {
				ignore_hoist!(stmt, hoisted;
					Import(span, glob, ident)
					Cpp(span, code)
				)
			}
		}
	}
	for stmt in for_later {
		match stmt {
			Stmt::Func(span, privacy, ident, func) => {
				let hoisted_func = func.hoist(Some(hoisted.clone()));
				hoisted
					.stmts_mut()
					.push(HoistedStmt::Func(span, privacy, ident, hoisted_func));
			}
			_ => unreachable!(),
		}
	}
	if !DIAGNOSTICS.lock().unwrap().is_empty() {
		let diagnostics = DIAGNOSTICS.lock().unwrap();
		Err(diagnostics.clone())
	} else {
		Ok(hoisted)
	}
}
