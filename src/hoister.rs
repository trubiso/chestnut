use crate::{
	parser::types::{Expr, Func, FuncAttribs, Ident, Privacy, Scope, Stmt, Type, TypedIdent},
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

#[derive(Debug, Clone)]
pub struct FuncSignature {
	pub generics: Vec<Ident>,
	pub arg_tys: Vec<Type>,
	pub return_ty: Type,
}

#[derive(Debug, Default, Clone)]
pub struct MadeTypeSignature {
	pub fields: HashMap<String, Type>,
	pub funcs: HashMap<String, FuncSignature>,
}

#[derive(Debug, Clone)]
pub struct HoistedFunc {
	pub span: Span,
	pub return_ty: Type,
	pub args: Vec<TypedIdent>,
	pub generics: Vec<Ident>,
	pub body: HoistedScope,
	pub attribs: FuncAttribs,
	pub decl_span: Span,
}

impl HoistedFunc {
	pub fn signature(&self) -> FuncSignature {
		FuncSignature {
			generics: self.generics.clone(),
			arg_tys: self.args.iter().map(|x| x.ty.clone()).collect(),
			return_ty: self.return_ty.clone(),
		}
	}
}

#[derive(Debug, Clone)]
pub enum HoistedStmt {
	Create(Span, Privacy, TypedIdent, bool, Expr),
	Declare(Span, Privacy, TypedIdent, bool),
	Set(Span, Ident, Expr),
	Func(Span, Privacy, Ident, HoistedFunc),
	Return(Span, Expr),
	Class(
		Span,
		Privacy,
		Ident,
		Vec<Ident>, /* generics */
		Span,
		HoistedScope,
	),
	Import(Span, bool, Ident),
	BareExpr(Span, Expr),
	Unsafe(Span, HoistedScope),
	Cpp(Span, String),
}

#[derive(Debug, Default, Clone)]
pub struct HoistedScope {
	pub vars: HashMap<String, Type>,
	pub var_spans: HashMap<String, Span>,
	pub funcs: HashMap<String, FuncSignature>,
	pub func_spans: HashMap<String, Span>,
	pub tys: HashMap<String, MadeTypeSignature>,
	pub ty_spans: HashMap<String, Span>,
	pub stmts: Vec<HoistedStmt>,
}

impl Func {
	pub fn hoist(self, inherit: Option<HoistedScope>) -> HoistedFunc {
		HoistedFunc {
			span: self.span,
			return_ty: self.return_ty,
			args: self.args,
			generics: self.generics,
			body: hoist(self.body, inherit).unwrap_or_default(),
			attribs: self.attribs,
			decl_span: self.decl_span,
		}
	}
}

macro_rules! ignore_hoist {
	($x:expr;$($v:ident($($p:tt),*))*) => {
		match $x {
			$(Stmt::$v($($p),*) => HoistedStmt::$v($($p),*),)*
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
	scope: Scope,
	inherit: Option<HoistedScope>,
) -> Result<HoistedScope, Vec<Diagnostic<usize>>> {
	let mut hoisted = inherit.unwrap_or_else(HoistedScope::default);
	for stmt in scope.stmts {
		hoisted.stmts.push(match stmt {
			Stmt::Create(span, privacy, ty_ident, is_mut, expr) => {
				if hoisted.vars.contains_key(&ty_ident.ident_str()) {
					let decl_span = hoisted.var_spans.get(&ty_ident.ident_str()).unwrap();
					redeclaration_error("variable", &span, decl_span);
				}
				hoisted
					.vars
					.insert(ty_ident.ident_str(), ty_ident.ty.clone());
				hoisted.var_spans.insert(ty_ident.ident_str(), span.clone());
				HoistedStmt::Create(span, privacy, ty_ident, is_mut, expr)
			}
			Stmt::Declare(span, privacy, ty_ident, is_mut) => {
				if hoisted.vars.contains_key(&ty_ident.ident_str()) {
					let decl_span = hoisted.var_spans.get(&ty_ident.ident_str()).unwrap();
					redeclaration_error("variable", &span, decl_span);
				}
				hoisted
					.vars
					.insert(ty_ident.ident_str(), ty_ident.ty.clone());
				hoisted.var_spans.insert(ty_ident.ident_str(), span.clone());
				HoistedStmt::Declare(span, privacy, ty_ident, is_mut)
			}
			Stmt::Set(span, ident, expr) => {
				if !hoisted.vars.contains_key(&ident.to_string()) {
					// TODO: add diagnostic (non-existing variable)
				}
				HoistedStmt::Set(span, ident, expr)
			}
			Stmt::Func(span, privacy, ident, func) => {
				if hoisted.funcs.contains_key(&ident.to_string()) {
					let decl_span = hoisted.func_spans.get(&ident.to_string()).unwrap();
					redeclaration_error("function", &func.decl_span, decl_span);
				}
				// TODO: this is stupid, as it will not hoist fully in the top level
				let hoisted_func = func.hoist(Some(hoisted.clone()));
				hoisted
					.funcs
					.insert(ident.to_string(), hoisted_func.signature());
				hoisted.func_spans.insert(ident.to_string(), span.clone());
				HoistedStmt::Func(span, privacy, ident, hoisted_func)
			}
			Stmt::Class(span, privacy, ident, generics, decl_span, body) => {
				if hoisted.tys.contains_key(&ident.to_string()) {
					let old_decl_span = hoisted.ty_spans.get(&ident.to_string()).unwrap();
					redeclaration_error("type", &decl_span, old_decl_span);
				}
				// TODO: this is stupid, as it will not hoist fully in the top level
				let hoisted_scope = hoist(body, Some(hoisted.clone())).unwrap_or_default();
				// TODO: get the actual made type signature
				hoisted.tys.insert(ident.to_string(), MadeTypeSignature::default());
				hoisted.ty_spans.insert(ident.to_string(), decl_span.clone());
				HoistedStmt::Class(span, privacy, ident, generics, decl_span, hoisted_scope)
			}
			Stmt::Unsafe(span, scope) => {
				let hoisted_scope = hoist(scope, Some(hoisted.clone())).unwrap_or_default();
				HoistedStmt::Unsafe(span, hoisted_scope)
			}
			stmt => {
				ignore_hoist!(stmt;
					Return(span, value)
					Import(span, glob, ident)
					BareExpr(span, expr)
					Cpp(span, code)
				)
			}
		})
	}
	if !DIAGNOSTICS.lock().unwrap().is_empty() {
		let diagnostics = DIAGNOSTICS.lock().unwrap();
		Err(diagnostics.clone())
	} else {
		Ok(hoisted)
	}
}
