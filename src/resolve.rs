use crate::parser::types::{Expr, Func, Ident, Scope, Stmt, Type, TypedIdent};
use codespan_reporting::diagnostic::{Diagnostic, Label};
use lazy_static::lazy_static;
use std::{collections::HashMap, sync::Mutex};

#[derive(Debug, Default, Clone)]
pub struct ResolvedScope {
	// TODO: assign each var a usize to know declaration order.
	// funcs don't need order, they can be arbitrarily defined.
	pub vars: HashMap<String, ResolvedVar>,
	pub funcs: HashMap<String, ResolvedFunc>,
}

lazy_static! {
	static ref DIAGNOSTICS: Mutex<Vec<Diagnostic<usize>>> = Mutex::new(vec![]);
}

pub fn add_diagnostic(diagnostic: Diagnostic<usize>) {
	DIAGNOSTICS.lock().unwrap().push(diagnostic);
}

impl ResolvedScope {
	pub fn add_var(&mut self, ty_ident: TypedIdent, value: Option<Expr>) {
		if ty_ident.ident.is_discarded() {
			return;
		}
		let name = ty_ident.ident_str();
		let ty = ty_ident.ty;
		self.vars
			.insert(name.clone(), ResolvedVar { name, ty, value });
	}

	pub fn set_var(&mut self, ident: Ident, expr: Expr) {
		if ident.is_discarded() {
			return;
		} // TODO: ?
		if let Some(x) = self.vars.get_mut(&ident.to_string()) {
			if !matches!(&x.ty, Type::Mut(_, _)) {
				let span = x.ty.span();
				add_diagnostic(
					Diagnostic::error()
						.with_message("tried to set non-mut symbol")
						.with_labels(vec![Label::primary(span.file_id, span.range())]),
				);
				return;
			}
			x.value = Some(expr);
		} else {
			let span = ident.span() + expr.span();
			add_diagnostic(
				Diagnostic::error()
					.with_message("tried to set non-existing symbol")
					.with_labels(vec![Label::primary(span.file_id, span.range())]),
			);
		}
	}

	pub fn set_func(&mut self, ident: Ident, func: Func) {
		let mut args = vec![];
		for arg in func.args {
			args.push(ResolvedArg {
				name: arg.ident_str(),
				ty: arg.ty,
			});
		}

		self.funcs.insert(
			ident.to_string(),
			ResolvedFunc {
				name: ident.to_string(),
				args,
			},
		);
	}

	pub fn check_ident_exists(&self, ident: Ident) {
		let vars_has = self.vars.contains_key(&ident.to_string());
		let funcs_has = self.funcs.contains_key(&ident.to_string());
		if !vars_has && !funcs_has {
			let span = ident.span();
			add_diagnostic(
				Diagnostic::error()
					.with_message("tried to access non-existing symbol")
					.with_labels(vec![Label::primary(span.file_id, span.range())
						.with_message(format!("couldn't find symbol in current scope"))]),
			)
		}
	}

	pub fn check_expr(&self, expr: Expr) {
		match expr {
			Expr::CharLiteral(_, _) => {}
			Expr::StringLiteral(_, _) => {}
			Expr::NumberLiteral(_, _) => {}
			Expr::Identifier(_, ident) => {
				self.check_ident_exists(ident);
			}
			Expr::BinaryOp(_, lhs, _op, rhs) => {
				self.check_expr(*lhs);
				self.check_expr(*rhs);
			}
			Expr::UnaryOp(_, _op, val) => {
				self.check_expr(*val);
			}
			Expr::Lambda(_, _func) => {
				// TODO: resolve lambda
			}
			Expr::Call(_, ident, args) => {
				self.check_expr(*ident);
				for arg in args {
					self.check_expr(arg);
				}
			}
			Expr::Error(_) => panic!("???"),
		}
	}
}

#[derive(Debug, Clone)]
pub struct ResolvedVar {
	pub name: String,
	pub ty: Type,            // TODO: ResolvedTy
	pub value: Option<Expr>, // NOTE: in the future we might support uninitialized variables
}

#[derive(Debug, Clone)]
pub struct ResolvedFunc {
	pub name: String,
	pub args: Vec<ResolvedArg>,
}

#[derive(Debug, Clone)]
pub struct ResolvedArg {
	pub name: String,
	pub ty: Type, // TODO: ResolvedTy
}

pub fn resolve(
	scope: Scope,
	is_func: bool,
	inherit_scope: Option<ResolvedScope>,
) -> Result<ResolvedScope, Vec<Diagnostic<usize>>> {
	let mut resolved_scope = ResolvedScope::default();
	if let Some(scope) = inherit_scope {
		resolved_scope.funcs = scope.funcs;
		resolved_scope.vars = scope.vars;
	}
	let mut return_value = None;
	for stmt in scope.stmts {
		match stmt {
			Stmt::Create(_, ty_ident, expr) => {
				resolved_scope.check_expr(expr.clone());
				resolved_scope.add_var(ty_ident, Some(expr));
			}
			Stmt::Set(_, ident, expr) => {
				resolved_scope.check_ident_exists(ident.clone());
				resolved_scope.check_expr(expr.clone());
				resolved_scope.set_var(ident, expr);
			}
			Stmt::Func(_, ident, func) => {
				let mut frs = resolved_scope.clone();
				for arg in func.clone().args {
					frs.add_var(arg, None);
				}
				// TODO: use resolved scope
				let _ = resolve(func.clone().body, true, Some(frs));
				resolved_scope.set_func(ident, func);
			}
			Stmt::Return(span, expr) => {
				if !is_func {
					add_diagnostic(
						Diagnostic::error()
							.with_message("tried to return from non-function")
							.with_labels(vec![Label::primary(span.file_id, span.range())
								.with_message(format!("return statement in global scope"))]),
					);
				}
				resolved_scope.check_expr(expr.clone());
				return_value = Some(expr);
				break;
			}
			Stmt::BareExpr(_, expr) => {
				resolved_scope.check_expr(expr);
			}
		}
	}
	// TODO: idk how we should use this LOL this code is absolutely useless
	match return_value {
		Some(_) => {}
		None => {}
	}
	if !DIAGNOSTICS.lock().unwrap().is_empty() {
		let diagnostics = DIAGNOSTICS.lock().unwrap();
		Err(diagnostics.clone())
	} else {
		Ok(resolved_scope)
	}
}
