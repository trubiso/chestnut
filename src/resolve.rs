use std::collections::HashMap;

use crate::parser::types::{Expr, Func, Ident, Scope, Stmt, Type, TypedIdent};

#[derive(Debug, Default, Clone)]
pub struct ResolvedScope {
	pub vars: HashMap<String, ResolvedVar>,
	pub funcs: HashMap<String, ResolvedFunc>,
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
		self.vars.get_mut(&ident.to_string()).unwrap().value = Some(expr);
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
			
			panic!("Ident {ident} not found in scope!");
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

pub fn resolve(scope: Scope, is_func: bool, inherit_scope: Option<ResolvedScope>) -> ResolvedScope {
	println!("resolving scope!");
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
				resolve(func.clone().body, true, Some(frs));
				resolved_scope.set_func(ident, func);
			}
			Stmt::Return(_, expr) => {
				if !is_func {
					panic!("Returned from non-func");
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
	println!("{return_value:?}");
	// println!("{resolved_scope:?}")
	resolved_scope
}
