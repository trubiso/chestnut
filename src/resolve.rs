use crate::{
	parser::types::{BuiltinType, Expr, Func, Ident, Privacy, Scope, Stmt, Type, TypedIdent, FuncAttribs},
	span::Span,
};
use codespan_reporting::diagnostic::{Diagnostic, Label};
use derive_more::Display;
use lazy_static::lazy_static;
use std::{cmp::Ordering, collections::HashMap, sync::Mutex};

#[derive(Debug, Clone)]
pub enum ResolvedStmt {
	Create(Span, Privacy, TypedIdent, Expr),
	Declare(Span, Privacy, TypedIdent),
	Set(Span, Ident, Expr),
	Return(Span, Expr),
	BareExpr(Span, Expr),
}

impl ResolvedStmt {
	pub fn span(&self) -> Span {
		match self {
			Self::Create(x, _, _, _)
			| Self::Declare(x, _, _)
			| Self::Set(x, _, _)
			| Self::Return(x, _)
			| Self::BareExpr(x, _) => x.clone(),
		}
	}
}

#[derive(Debug, Default, Clone)]
pub struct InheritableData {
	pub vars: HashMap<String, ResolvedVar>,
	pub var_spans: HashMap<String, Span>,
	// TODO: top level analyzer
	// TODO: function overloads
	pub funcs: HashMap<String, ResolvedFunc>,
	pub func_spans: HashMap<String, Span>,
	pub types: HashMap<String, ResolvedType>,
	pub type_spans: HashMap<String, Span>,
	// TODO: remove scopes from type & func spans
}

impl std::ops::Add for InheritableData {
	type Output = InheritableData;

	fn add(self, rhs: Self) -> Self::Output {
		Self {
			vars: rhs.vars.into_iter().chain(self.vars).collect(),
			var_spans: rhs.var_spans.into_iter().chain(self.var_spans).collect(),
			funcs: rhs.funcs.into_iter().chain(self.funcs).collect(),
			func_spans: rhs.func_spans.into_iter().chain(self.func_spans).collect(),
			types: rhs.types.into_iter().chain(self.types).collect(),
			type_spans: rhs.type_spans.into_iter().chain(self.type_spans).collect(),
		}
	}
}

#[derive(Debug, Default, Clone)]
pub struct ResolvedScope {
	pub data: InheritableData,
	pub inherit: InheritableData,
	pub stmts: Vec<ResolvedStmt>,
}

lazy_static! {
	static ref DIAGNOSTICS: Mutex<Vec<Diagnostic<usize>>> = Mutex::new(vec![]);
}

pub fn add_diagnostic(diagnostic: Diagnostic<usize>) {
	DIAGNOSTICS.lock().unwrap().push(diagnostic);
}

macro_rules! get_datum {
	($name:ident $nmut:ident $nhas:ident => $ident:ident ($ty:ty)) => {
		#[allow(unused)]
		fn $name(&self, name: &str) -> Option<&$ty> {
			match self.data.$ident.get(name) {
				Some(x) => Some(x),
				None => self.inherit.$ident.get(name),
			}
		}

		#[allow(unused)]
		fn $nmut(&mut self, name: &str) -> Option<&mut $ty> {
			match self.data.$ident.get_mut(name) {
				Some(x) => Some(x),
				None => self.inherit.$ident.get_mut(name),
			}
		}

		#[allow(unused)]
		fn $nhas(&self, name: &str) -> bool {
			self.data.$ident.contains_key(name) || self.inherit.$ident.contains_key(name)
		}
	};
}

impl ResolvedScope {
	get_datum!(get_type get_type_mut has_type => types (ResolvedType));
	get_datum!(get_type_span get_type_span_mut has_type_span => type_spans (Span));
	get_datum!(get_var get_var_mut has_var => vars (ResolvedVar));
	get_datum!(get_var_span get_var_span_mut has_var_span => var_spans (Span));
	get_datum!(get_func get_func_mut has_func => funcs (ResolvedFunc));
	get_datum!(get_func_span get_func_span_mut has_func_span => func_spans (Span));

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
				} else if let Some(ty) = self.get_type(&x.ident.to_string()) {
					if ty.generic_count != x.generics.len() {
						let decl_span = self.get_type_span(&x.ident.to_string()).unwrap();
						add_diagnostic(
							Diagnostic::error()
								.with_message(match x.generics.len().cmp(&ty.generic_count) {
									Ordering::Less => "not enough generics in type",
									Ordering::Greater => "too many generics in type",
									_ => unreachable!(),
								})
								.with_labels(vec![
									Label::primary(span.file_id, span.range()),
									Label::secondary(decl_span.file_id, decl_span.range())
										.with_message("original declaration here"),
								]),
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
					self.check_type(generic);
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

	pub fn add_var(&mut self, span: Span, ty_ident: TypedIdent, value: Option<Expr>) {
		if ty_ident.ident.is_discarded() {
			return;
		}
		let name = ty_ident.ident_str();
		let ty = ty_ident.ty;
		self.data.vars.insert(
			name.clone(),
			ResolvedVar {
				name: name.clone(),
				ty,
				value,
			},
		);
		self.data.var_spans.insert(name, span);
	}

	pub fn add_type(&mut self, span: Span, name: String, ty: ResolvedType) {
		self.data.types.insert(name.clone(), ty);
		self.data.type_spans.insert(name, span);
	}

	pub fn set_var(&mut self, ident: Ident, expr: Expr) {
		if ident.is_discarded() {
			return;
		}
		if let Some(x) = self.get_var_mut(&ident.to_string()) {
			if !matches!(&x.ty, Type::Mut(_, _)) {
				let span = ident.span() + expr.span();
				let var_span = self.get_var_span(&ident.to_string()).unwrap();
				add_diagnostic(
					Diagnostic::error()
						.with_message("tried to set non-mut symbol")
						.with_labels(vec![
							Label::primary(span.file_id, span.range()),
							Label::secondary(var_span.file_id, var_span.range())
								.with_message("original declaration here"),
						]),
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

	pub fn add_func(
		&mut self,
		span: Span,
		ident: Ident,
		func: Func,
		return_ty: Type,
		body: ResolvedScope,
		attribs: FuncAttribs,
	) {
		let mut args = vec![];
		for arg in func.args {
			args.push(ResolvedArg {
				name: arg.ident_str(),
				ty: arg.ty,
			});
		}

		self.data.funcs.insert(
			ident.to_string(),
			ResolvedFunc {
				name: ident.to_string(),
				args,
				return_ty,
				body,
				attribs,
			},
		);
		self.data.func_spans.insert(ident.to_string(), span);
	}

	pub fn check_ident_exists(&self, ident: Ident) {
		let vars_has = self.has_var(&ident.to_string());
		let funcs_has = self.has_func(&ident.to_string());
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
				// TODO: add error if it's not initialized
				self.check_ident_exists(ident);
			}
			Expr::BinaryOp(_, lhs, _op, rhs) => {
				self.check_expr(*lhs.clone());
				self.check_expr(*rhs.clone());
				// this may seem useless but it checks for types in binary ops
				self.get_expr_ty(*lhs);
				self.get_expr_ty(*rhs);
			}
			Expr::UnaryOp(_, _op, val) => {
				self.check_expr(*val);
			}
			Expr::Lambda(_, _func) => {
				// TODO: resolve lambda
			}
			Expr::Call(span, func_expr, args) => {
				self.check_expr(*func_expr.clone());
				let func_expr_span = func_expr.span();
				let (func, decl_span) = if let Expr::Identifier(_, name) = *func_expr {
					self.check_ident_exists(name.clone());
					let Some(func) = self.get_func(&name.to_string()) else { return; };
					(
						func.clone(),
						self.get_func_span(&name.to_string())
							.cloned()
							.unwrap_or(func_expr_span),
					)
				} else if let Expr::Lambda(_, func) = *func_expr {
					// TODO: add args to inherit scope
					let (scope, ty) = resolve(func.body, Context::Func, None, None).unwrap();
					(
						ResolvedFunc {
							name: "~".into(),
							args: func
								.args
								.iter()
								.map(|x| ResolvedArg {
									name: x.ident.to_string(),
									ty: x.ty.clone(),
								})
								.collect(),
							return_ty: ty,
							body: scope,
							attribs: FuncAttribs::default(),
						},
						func.span,
					)
				} else {
					let span = func_expr.span();
					let ty = self.get_expr_ty(*func_expr);
					add_diagnostic(
						Diagnostic::error()
							.with_message("tried to call non-function")
							.with_labels(vec![Label::primary(span.file_id, span.range())
								.with_message(format!("expected function, found {ty}"))]),
					);
					return;
				};
				if func.args.len() != args.len() {
					add_diagnostic(
						Diagnostic::error()
							.with_message(match args.len().cmp(&func.args.len()) {
								Ordering::Less => "not enough arguments in function call",
								Ordering::Greater => "too many arguments in function call",
								_ => unreachable!(),
							})
							.with_labels(vec![
								Label::primary(span.file_id, span.range()),
								Label::secondary(decl_span.file_id, decl_span.range())
									.with_message("original declaration here"),
							]),
					);
					return;
				}
				for (i, arg) in args.iter().enumerate() {
					self.check_expr(arg.clone());
					let arg_ty = self.get_expr_ty(arg.clone());
					let expected_ty = func.args[i].ty.clone();
					if arg_ty != expected_ty {
						add_diagnostic(
							Diagnostic::error()
								.with_message("incorrect argument type")
								.with_labels(vec![
									Label::primary(arg_ty.span().file_id, arg_ty.span().range())
										.with_message(format!(
											"found {arg_ty}, expected {expected_ty}"
										)),
									Label::secondary(decl_span.file_id, decl_span.range())
										.with_message("original declaration here"),
								]),
						)
					}
				}
			}
			Expr::Error(_) => panic!("???"),
		}
	}

	// TODO: be lazier with int literals and automatically cast them in Stmt::Create
	pub fn get_expr_ty(&self, expr: Expr) -> Type {
		match expr {
			Expr::CharLiteral(span, _) => Type::Builtin(span, BuiltinType::Char),
			Expr::StringLiteral(span, _) => Type::Builtin(span, BuiltinType::String),
			Expr::NumberLiteral(span, literal) => Type::Builtin(span, literal.as_ty()),
			Expr::Identifier(span, ident) => self
				.get_var(&ident.to_string())
				.map(|x| x.ty.clone())
				.unwrap_or_else(|| Type::Builtin(span, BuiltinType::Error)),
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
				lhs_ty
			}
			// TODO: &x, *x => ref + deref. these would return non-reffed or reffed tys.
			Expr::UnaryOp(_span, _op, val) => self.get_expr_ty(*val),
			Expr::Lambda(span, _func) => {
				// TODO: resolve lambda
				Type::Builtin(span, BuiltinType::Error)
			}
			Expr::Call(span, ident, _args) => self
				.get_func(&ident.to_string())
				.map(|x| x.return_ty.clone())
				.unwrap_or_else(|| {
					// TODO: ???
					let Expr::Lambda(_, func) = *ident else { return Type::Builtin(span, BuiltinType::Error); };
					func.return_ty
				}),
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
	pub body: Option<ResolvedScope>, // if it's a generic no body defines it
}

#[derive(Debug, Clone)]
pub struct ResolvedFunc {
	pub name: String,
	pub args: Vec<ResolvedArg>,
	pub return_ty: Type,
	pub body: ResolvedScope,
	pub attribs: FuncAttribs,
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
	expected_func_ty: Option<(Span, Span, Type)>,
) -> Result<(ResolvedScope, Type), Vec<Diagnostic<usize>>> {
	let mut resolved_scope = ResolvedScope::default();
	if let Some(scope) = inherit_scope {
		resolved_scope.inherit = scope.data + scope.inherit;
	}
	let mut return_ty = Type::Builtin(scope.span, BuiltinType::Void);
	let mut return_span = None;
	for stmt in scope.stmts {
		match stmt {
			Stmt::Create(span, privacy, mut ty_ident, expr) => {
				check_privacy(privacy.clone(), context.clone());
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
				resolved_scope.add_var(span.clone(), ty_ident.clone(), Some(expr.clone()));
				if !ty_ident.ident.is_discarded() {
					resolved_scope
						.stmts
						.push(ResolvedStmt::Create(span, privacy, ty_ident, expr));
				} else {
					// TODO: might throw some c++ warnings
					resolved_scope
						.stmts
						.push(ResolvedStmt::BareExpr(span, expr));
				}
			}
			Stmt::Declare(span, privacy, ty_ident) => {
				// TODO: add error if the type is ~
				check_privacy(privacy.clone(), context.clone());
				resolved_scope.check_type(ty_ident.ty.clone());
				resolved_scope.add_var(span.clone(), ty_ident.clone(), None);
				resolved_scope
					.stmts
					.push(ResolvedStmt::Declare(span, privacy, ty_ident));
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
				resolved_scope.set_var(ident.clone(), expr.clone());
				resolved_scope
					.stmts
					.push(ResolvedStmt::Set(span, ident, expr));
			}
			Stmt::Func(span, privacy, ident, func) => {
				check_privacy(privacy, context.clone());
				let mut frs = resolved_scope.clone();
				for generic in &func.generics {
					let name = generic.to_string();
					frs.add_type(
						generic.span(),
						name.clone(),
						ResolvedType {
							name,
							generic_count: 0,
							fields: HashMap::new(), /* NOTE: potentially in the future we will
							                         * change this */
							funcs: HashMap::new(),
							body: None,
						},
					);
				}
				frs.check_type(func.return_ty.clone());
				for arg in &func.args {
					frs.check_type(arg.ty.clone());
					frs.add_var(arg.span.clone(), arg.clone(), None);
				}
				// TODO: use resolved scope
				let mut return_ty = func.return_ty.clone();
				let body = if return_ty.is_inferred() {
					match resolve(func.body.clone(), Context::Func, Some(frs), None) {
						Ok((scope, ty)) => {
							return_ty = ty;
							scope
						}
						Err(_) => ResolvedScope::default(),
					}
				} else {
					match resolve(
						func.body.clone(),
						Context::Func,
						Some(frs),
						Some((ident.span(), return_ty.span(), return_ty.clone())),
					) {
						Ok((scope, _)) => scope,
						Err(_) => ResolvedScope::default(),
					}
				};
				let attribs = func.attribs.clone();
				resolved_scope.add_func(span, ident, func, return_ty, body, attribs);
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
				return_ty = resolved_scope.get_expr_ty(expr.clone());
				return_span = Some(span.clone());
				resolved_scope.stmts.push(ResolvedStmt::Return(span, expr));
			}
			Stmt::Class(span, privacy, ident, generics, body) => {
				check_privacy(privacy, context.clone());
				let name = ident.to_string();
				let mut crs = resolved_scope.clone();
				let mut ty = ResolvedType {
					name: name.clone(),
					generic_count: generics.len(),
					fields: HashMap::new(), // TODO
					funcs: HashMap::new(),  // TODO
					body: None,
				};
				crs.add_type(span.clone(), name.clone(), ty.clone());
				for generic in generics {
					let name = generic.to_string();
					crs.add_type(
						generic.span(),
						name.clone(),
						ResolvedType {
							name,
							generic_count: 0,
							fields: HashMap::new(), /* NOTE: potentially in the future we will
							                         * change this */
							funcs: HashMap::new(),
							body: None,
						},
					)
				}
				let scope = match resolve(body.clone(), Context::Class, Some(crs), None) {
					Ok((scope, _)) => scope,
					Err(_) => ResolvedScope::default(),
				};
				ty.body = Some(scope);
				resolved_scope.add_type(span, name, ty);
			}
			Stmt::BareExpr(span, expr) => {
				resolved_scope.check_expr(expr.clone());
				resolved_scope
					.stmts
					.push(ResolvedStmt::BareExpr(span, expr));
			}
		}
	}
	if let Some((func_span, return_ty_span, expected_ty)) = expected_func_ty {
		if return_ty != expected_ty {
			let span = return_span.unwrap_or(func_span);
			if return_ty.is_void() {
				add_diagnostic(
					Diagnostic::error()
						.with_message("no return in non-void function")
						.with_labels(vec![
							Label::primary(span.file_id, span.range()),
							Label::secondary(return_ty_span.file_id, return_ty_span.range())
								.with_message("return type declared here"),
						]),
				)
			} else {
				add_diagnostic(
					Diagnostic::error()
						.with_message("incorrect return type")
						.with_labels(vec![Label::primary(span.file_id, span.range())])
						.with_notes(vec![format!("expected {expected_ty}, got {return_ty}")]),
				)
			}
		}
	}
	if !DIAGNOSTICS.lock().unwrap().is_empty() {
		let diagnostics = DIAGNOSTICS.lock().unwrap();
		Err(diagnostics.clone())
	} else {
		Ok((resolved_scope, return_ty))
	}
}
