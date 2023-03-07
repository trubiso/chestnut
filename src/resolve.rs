use crate::parser::types::{
	BuiltinType, Expr, Func, Generic, Ident, Privacy, Scope, Stmt, Type, TypedIdent,
};
use codespan_reporting::diagnostic::{Diagnostic, Label};
use derive_more::Display;
use lazy_static::lazy_static;
use std::{cmp::Ordering, collections::HashMap, sync::Mutex};

#[derive(Debug, Default, Clone)]
pub struct ResolvedScope {
	// TODO: assign each var a usize to know declaration order.
	// funcs don't need order, they can be arbitrarily defined.
	pub vars: HashMap<String, ResolvedVar>,
	// TODO: top level analyzer
	pub funcs: HashMap<String, ResolvedFunc>,
	pub types: HashMap<String, ResolvedType>,
}

lazy_static! {
	static ref DIAGNOSTICS: Mutex<Vec<Diagnostic<usize>>> = Mutex::new(vec![]);
}

pub fn add_diagnostic(diagnostic: Diagnostic<usize>) {
	DIAGNOSTICS.lock().unwrap().push(diagnostic);
}

impl ResolvedScope {
	pub fn check_type(&self, ty: Type) {
		match ty {
			Type::BareType(span, x) => {
				if [
					"i8", "i16", "i32", "i64", "i128", "iz", "u8", "u16", "u32", "u64", "u128",
					"uz", "f16", "f32", "f64", "f128", "void", "bool", "string", "char",
				]
				.contains(&x.ident.to_string().as_str())
				{
					if !x.generics.is_empty() {
						add_diagnostic(
							Diagnostic::error()
								.with_message("tried to use generics on builtin")
								.with_labels(vec![Label::primary(span.file_id, span.range())]),
						)
					}
				} else if let Some(ty) = self.types.get(&x.ident.to_string()) {
					if ty.generic_count != x.generics.len() {
						add_diagnostic(
							Diagnostic::error()
								.with_message(match x.generics.len().cmp(&ty.generic_count) {
									Ordering::Less => "not enough generics in type",
									Ordering::Greater => "too many generics in type",
									_ => unreachable!(),
								})
								.with_labels(vec![Label::primary(span.file_id, span.range())]),
							// TODO: add note at the original declaration
						);
					}
				} else {
					add_diagnostic(
						Diagnostic::error()
							.with_message("tried to use undeclared type")
							.with_labels(vec![Label::primary(span.file_id, span.range())]),
					);
				}
				for generic in x.generics {
					let Generic::Type(ty) = generic else { panic!("haha you fell into the trap enum") };
					self.check_type(ty);
				}
			}
			Type::Ref(_, x) | Type::Mut(_, x) | Type::Optional(_, x) => self.check_type(*x),
			Type::Array(_, x, size) => {
				if let Some(x) = size {
					self.check_expr(*x)
				}
				self.check_type(*x);
			}
			Type::Inferred(_) => {}
			// we might have to remove inferred in some cases because type inference hehehe
			Type::Builtin(_, _) => {}
		}
	}

	pub fn add_var(&mut self, ty_ident: TypedIdent, value: Option<Expr>) {
		if ty_ident.ident.is_discarded() {
			return;
		}
		let name = ty_ident.ident_str();
		let ty = ty_ident.ty;
		self.vars
			.insert(name.clone(), ResolvedVar { name, ty, value });
	}

	pub fn add_type(&mut self, name: String, ty: ResolvedType) {
		self.types.insert(name, ty);
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
					// TODO: add note at the original variable declaration
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

	pub fn set_func(&mut self, ident: Ident, func: Func, return_ty: Type) {
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
				return_ty,
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
						.with_message("couldn't find symbol in current scope")]),
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
					// TODO: check arg types
				}
			}
			Expr::Error(_) => panic!("???"),
		}
	}

	pub fn get_expr_ty(&self, expr: Expr) -> Type {
		match expr {
			Expr::CharLiteral(span, _) => Type::Builtin(span, BuiltinType::Char),
			Expr::StringLiteral(span, _) => Type::Builtin(span, BuiltinType::String),
			Expr::NumberLiteral(span, literal) => Type::Builtin(span, literal.as_ty()),
			Expr::Identifier(_span, ident) => self.vars.get(&ident.to_string()).unwrap().ty.clone(),
			Expr::BinaryOp(span, lhs, _op, rhs) => {
				let lhs_span = lhs.span();
				let rhs_span = rhs.span();
				let lhs_ty = self.get_expr_ty(*lhs);
				let rhs_ty = self.get_expr_ty(*rhs);
				if lhs_ty != rhs_ty {
					add_diagnostic(
						Diagnostic::error()
							.with_message("operating with incompatible types")
							.with_labels(vec![
								Label::primary(span.file_id, span.range()),
								Label::secondary(lhs_span.file_id, lhs_span.range())
									.with_message(format!("type is {lhs_ty}")),
								Label::secondary(rhs_span.file_id, rhs_span.range())
									.with_message(format!("type is {rhs_ty}")),
							]),
					);
				}
				lhs_ty // TODO: support returning different type from operator (?)
			}
			// TODO: do something with operator
			Expr::UnaryOp(_span, _op, val) => self.get_expr_ty(*val),
			Expr::Lambda(span, _func) => {
				// TODO: resolve lambda
				Type::Builtin(span, BuiltinType::Error)
			}
			Expr::Call(_span, ident, _args) => self
				.funcs
				.get(&ident.to_string())
				.unwrap()
				.return_ty
				.clone(),
			Expr::Error(_) => panic!("???"),
		}
	}
}

#[derive(Debug, Clone)]
pub struct ResolvedVar {
	pub name: String,
	pub ty: Type,            // TODO: should we make another type?
	pub value: Option<Expr>, // NOTE: in the future we might support uninitialized variables
}

#[derive(Debug, Clone)]
pub struct ResolvedType {
	pub name: String,
	pub generic_count: usize, // TODO: ResolvedGeneric
	pub fields: HashMap<String, ResolvedType>,
	pub funcs: HashMap<String, ResolvedFunc>,
}

#[derive(Debug, Clone)]
pub struct ResolvedFunc {
	pub name: String,
	pub args: Vec<ResolvedArg>,
	pub return_ty: Type,
}

#[derive(Debug, Clone)]
pub struct ResolvedArg {
	pub name: String,
	pub ty: Type, // TODO: ResolvedTy
}

#[derive(Debug, Display, Clone, PartialEq, Eq)]
pub enum Context {
	#[display(fmt = "top level")]
	TopLevel,
	#[display(fmt = "class")]
	Class,
	#[display(fmt = "func")]
	Func,
}

fn check_privacy(privacy: Privacy, context: Context) {
	let is_valid = privacy.is_default()
		|| match context {
			Context::TopLevel => privacy.is_export(),
			Context::Class => !privacy.is_export(),
			Context::Func => false,
		};
	if !is_valid {
		let span = privacy.span().unwrap(); // never Default => never None
		add_diagnostic(
			Diagnostic::error()
				.with_message(format!(
					"invalid privacy qualifier {privacy}in {context} context"
				))
				.with_labels(vec![Label::primary(span.file_id, span.range())]),
		);
	}
}

pub fn resolve(
	scope: Scope,
	context: Context,
	inherit_scope: Option<ResolvedScope>,
) -> Result<ResolvedScope, Vec<Diagnostic<usize>>> {
	let mut resolved_scope = ResolvedScope::default();
	if let Some(scope) = inherit_scope {
		resolved_scope.funcs = scope.funcs;
		resolved_scope.vars = scope.vars;
		resolved_scope.types = scope.types;
	}
	let mut return_value = None;
	for stmt in scope.stmts {
		match stmt {
			Stmt::Create(span, privacy, mut ty_ident, expr) => {
				check_privacy(privacy, context.clone());
				resolved_scope.check_type(ty_ident.ty.clone());
				resolved_scope.check_expr(expr.clone());
				let lhs = ty_ident.ty.clone();
				let rhs = resolved_scope.get_expr_ty(expr.clone());
				let lhs_span = ty_ident.ty.span();
				let rhs_span = expr.span();
				if !lhs.is_inferred() && lhs != rhs {
					add_diagnostic(
						Diagnostic::error()
							.with_message("assignment between incompatible types")
							.with_labels(vec![
								Label::primary(span.file_id, span.range()),
								Label::secondary(lhs_span.file_id, lhs_span.range())
									.with_message(format!("type is {lhs}")),
								Label::secondary(rhs_span.file_id, rhs_span.range())
									.with_message(format!("type is {rhs}")),
							]),
					)
				}
				ty_ident.ty = match ty_ident.ty {
					Type::Mut(_, _) => Type::Mut(lhs_span, Box::new(rhs)),
					_ => rhs,
				};
				resolved_scope.add_var(ty_ident, Some(expr));
			}
			Stmt::Declare(_, privacy, ty_ident) => {
				check_privacy(privacy, context.clone());
				resolved_scope.check_type(ty_ident.ty.clone());
				resolved_scope.add_var(ty_ident, None);
			}
			Stmt::Set(span, ident, expr) => {
				resolved_scope.check_ident_exists(ident.clone());
				resolved_scope.check_expr(expr.clone());
				let lhs_span = ident.span();
				let lhs =
					resolved_scope.get_expr_ty(Expr::Identifier(lhs_span.clone(), ident.clone()));
				let rhs = resolved_scope.get_expr_ty(expr.clone());
				let rhs_span = expr.span();
				if lhs != rhs {
					add_diagnostic(
						Diagnostic::error()
							.with_message("assignment between incompatible types")
							.with_labels(vec![
								Label::primary(span.file_id, span.range()),
								Label::secondary(lhs_span.file_id, lhs_span.range())
									.with_message(format!("type is {lhs}")),
								Label::secondary(rhs_span.file_id, rhs_span.range())
									.with_message(format!("type is {rhs}")),
							]),
					)
				}
				resolved_scope.set_var(ident, expr);
			}
			Stmt::Func(_, privacy, ident, func) => {
				check_privacy(privacy, context.clone());
				// TODO: generics
				resolved_scope.check_type(func.return_ty.clone());
				let mut frs = resolved_scope.clone();
				for arg in func.args.clone() {
					resolved_scope.check_type(arg.ty.clone());
					frs.add_var(arg, None);
				}
				// TODO: use resolved scope
				let _ = resolve(func.body.clone(), Context::Func, Some(frs));
				let return_ty = func.return_ty.clone();
				resolved_scope.set_func(ident, func, return_ty);
			}
			Stmt::Return(span, expr) => {
				if context != Context::Func {
					add_diagnostic(
						Diagnostic::error()
							.with_message("tried to return from non-function")
							.with_labels(vec![Label::primary(span.file_id, span.range())
								.with_message("return statement in global scope")]),
					);
				}
				resolved_scope.check_expr(expr.clone());
				return_value = Some(expr);
				break;
			}
			Stmt::Class(_, privacy, ident, generics, body) => {
				check_privacy(privacy, context.clone());
				let name = ident.to_string();
				let mut crs = resolved_scope.clone();
				let ty = ResolvedType {
					name: name.clone(),
					generic_count: generics.len(),
					fields: HashMap::new(), // TODO
					funcs: HashMap::new(),  // TODO
				};
				crs.add_type(name.clone(), ty.clone());
				for generic in generics {
					let name = generic.to_string();
					crs.add_type(
						name.clone(),
						ResolvedType {
							name,
							generic_count: 0,
							fields: HashMap::new(), /* NOTE: potentially in the future we will
							                         * change this */
							funcs: HashMap::new(),
						},
					)
				}
				// TODO: use resolved scope
				let _ = resolve(body.clone(), Context::Class, Some(crs));
				resolved_scope.add_type(name, ty);
			}
			Stmt::BareExpr(_, expr) => {
				resolved_scope.check_expr(expr);
			}
		}
	}
	// TODO: idk how we should use this LOL this code is absolutely useless
	if return_value.is_some() {}
	if !DIAGNOSTICS.lock().unwrap().is_empty() {
		let diagnostics = DIAGNOSTICS.lock().unwrap();
		Err(diagnostics.clone())
	} else {
		Ok(resolved_scope)
	}
}
