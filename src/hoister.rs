use crate::{
	get_datum,
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

impl HoistedStmt {
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
}

#[derive(Debug, Default, Clone)]
pub struct HoistedScopeData {
	pub vars: HashMap<String, Type>,
	pub var_spans: HashMap<String, Span>,
	pub funcs: HashMap<String, FuncSignature>,
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
	get_datum!(get_var get_var_mut has_var add_var => vars (Type));
	get_datum!(get_var_span get_var_span_mut has_var_span add_var_span => var_spans (Span));
	get_datum!(get_func get_func_mut has_func add_func => funcs (FuncSignature));
	get_datum!(get_func_span get_func_span_mut has_func_span add_func_span => func_spans (Span));

	pub fn stmts(&self) -> &Vec<HoistedStmt> {
		&self.data.stmts
	}

	pub fn stmts_mut(&mut self) -> &mut Vec<HoistedStmt> {
		&mut self.data.stmts
	}
}

impl Func {
	pub fn hoist(self, inherit: Option<HoistedScope>) -> HoistedFunc {
		HoistedFunc {
			span: self.span,
			return_ty: self.return_ty,
			args: self.args,
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

impl Func {
	pub fn signature(&self) -> FuncSignature {
		FuncSignature {
			generics: self.generics.clone(),
			arg_tys: self.args.iter().map(|x| x.ty.clone()).collect(),
			return_ty: self.return_ty.clone(),
		}
	}
}

pub fn hoist(
	scope: Scope,
	inherit: Option<HoistedScope>,
) -> Result<HoistedScope, Vec<Diagnostic<usize>>> {
	let inherit_hoisted = inherit.unwrap_or_else(|| HoistedScope::new(scope.span.clone()));
	let mut hoisted = HoistedScope::new(scope.span);
	hoisted.inherit = inherit_hoisted.data + inherit_hoisted.inherit;
	let mut for_later = Vec::new();
	for stmt in scope.stmts {
		match stmt {
			Stmt::Create(span, privacy, ty_ident, is_mut, expr) => {
				if hoisted.has_var(&ty_ident.ident_str()) {
					let decl_span = hoisted.get_var_span(&ty_ident.ident_str()).unwrap();
					redeclaration_error("variable", &span, decl_span);
				}
				hoisted.add_var(&ty_ident.ident_str(), ty_ident.ty.clone());
				hoisted.add_var_span(&ty_ident.ident_str(), span.clone());
				hoisted
					.stmts_mut()
					.push(HoistedStmt::Create(span, privacy, ty_ident, is_mut, expr));
			}
			Stmt::Declare(span, privacy, ty_ident, is_mut) => {
				if hoisted.has_var(&ty_ident.ident_str()) {
					let decl_span = hoisted.get_var_span(&ty_ident.ident_str()).unwrap();
					redeclaration_error("variable", &span, decl_span);
				}
				hoisted.add_var(&ty_ident.ident_str(), ty_ident.ty.clone());
				hoisted.add_var_span(&ty_ident.ident_str(), span.clone());
				hoisted
					.stmts_mut()
					.push(HoistedStmt::Declare(span, privacy, ty_ident, is_mut));
			}
			Stmt::Set(span, ident, expr) => {
				if !hoisted.has_var(&ident.to_string()) {
					// TODO: add diagnostic (non-existing variable)
				}
				hoisted
					.stmts_mut()
					.push(HoistedStmt::Set(span, ident, expr));
			}
			Stmt::Func(span, privacy, ident, func) => {
				if hoisted.has_func(&ident.to_string()) {
					let decl_span = hoisted.get_func_span(&ident.to_string()).unwrap();
					redeclaration_error("function", &func.decl_span, decl_span);
				}
				hoisted.add_func(&ident.to_string(), func.signature());
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
				let hoisted_scope = hoist(scope, Some(hoisted.clone()))
					.unwrap_or_else(|_| HoistedScope::new(span.clone()));
				hoisted
					.stmts_mut()
					.push(HoistedStmt::Unsafe(span, hoisted_scope));
			}
			stmt => {
				ignore_hoist!(stmt, hoisted;
					Return(span, value)
					Import(span, glob, ident)
					BareExpr(span, expr)
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
