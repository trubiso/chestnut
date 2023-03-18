use crate::{
	common::{
		BareType, BuiltinType, Expr, Func, FuncSignature, Privacy, Scope, ScopeFmt, Stmt, Type,
		TypedIdent,
	},
	hoister::{
		HoistedBareType, HoistedExpr, HoistedFunc, HoistedFuncSignature, HoistedScope, HoistedStmt,
		HoistedType, HoistedTypedIdent, MadeTypeSignature,
	},
	lexer::Operator,
	parser::types::Ident,
	span::Span,
};
use codespan_reporting::diagnostic::{Diagnostic, Label};
use derive_more::Display;
use lazy_static::lazy_static;
use std::{cell::RefCell, cmp::Ordering, collections::HashMap, fmt, sync::Mutex};

use self::case::{check_case, Case};

pub mod case;

// TODO: FuncSignature (only types)
// TODO: ClassSignature (only types)

pub type ResolvedType = Type<ResolvedExpr>;
pub type ResolvedTypedIdent = TypedIdent<ResolvedType>;
pub type ResolvedBareType = BareType<ResolvedType>;
pub type ResolvedFunc = Func<ResolvedExpr, ResolvedScope>;
pub type ResolvedFuncSignature = FuncSignature<ResolvedType>;
pub type ResolvedExpr = Expr<ResolvedScope>;
pub type ResolvedStmt = Stmt<ResolvedExpr, ResolvedFunc, ResolvedScope>;

macro_rules! builtin {
	($s:expr, $v:ident) => {
		ResolvedType::Builtin($s, BuiltinType::$v)
	};
}

macro_rules! i_hate_partial_eq {
	($s:expr, $other:expr => $($v:ident($($f:ident),*)($($o:ident),*) => $c:expr;)*) => {
		match $s {
			$(
				ResolvedExpr::$v(_, $($f,)*) => {
					if let ResolvedExpr::$v(_, $($o,)*) = $other {
						$c
					} else {
						false
					}
				}
			)*
		}
	};
}

impl PartialEq for ResolvedExpr {
	fn eq(&self, other: &Self) -> bool {
		i_hate_partial_eq!(
			self, other =>
			CharLiteral(x)(y) => x == y;
			StringLiteral(x)(y) => x == y;
			NumberLiteral(x)(y) => x == y;
			Identifier(x)(y) => x == y;
			BinaryOp(a, b, c)(d, e, f) => (a.clone(), b, c.clone()) == (d.clone(), e, f.clone());
			UnaryOp(a, b)(c, d) => a == c && b == d;
			Lambda(f)(v) => f == v;
			Call(a, g, b)(c, j, d) => a == c && b == d && g == j;
			Dot(a, b)(c, d) => a == b && c == d;
		)
	}
}

impl Eq for ResolvedExpr {}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct InheritableData {
	pub vars: HashMap<String, ResolvedVar>,
	pub var_spans: HashMap<String, Span>,
	// TODO: function overloads
	pub funcs: HashMap<String, ResolvedFunc>,
	pub func_spans: HashMap<String, Span>,
	pub types: HashMap<String, ResolvedMadeType>,
	pub type_spans: HashMap<String, Span>,
	// TODO: remove scopes from type & func spans
}

#[derive(Debug, Clone)]
pub struct ResolvedScope {
	pub data: RefCell<InheritableData>,
	pub stmts: Vec<ResolvedStmt>,
	// TODO: we don't care about the stmts, only inherit and data
	pub old_hoisted: HoistedScope,
	pub span: Span,
}

impl fmt::Display for ResolvedScope {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		self.scope_fmt(f)
	}
}

impl Scope<ResolvedExpr> for ResolvedScope {
	fn stmts(&self) -> &Vec<Stmt<ResolvedExpr, crate::common::Func<ResolvedExpr, Self>, Self>>
	where
		Self: Sized,
	{
		&self.stmts
	}
}

lazy_static! {
	static ref DIAGNOSTICS: Mutex<Vec<Diagnostic<usize>>> = Mutex::new(vec![]);
}

pub fn add_diagnostic(diagnostic: Diagnostic<usize>) {
	DIAGNOSTICS.lock().unwrap().push(diagnostic);
}

macro_rules! get_datum {
	($nget:ident $nmut:ident $nhas:ident $nadd:ident => $ident:ident ($ty:ty)) => {
		pub fn $nget(&self, name: &str) -> Option<::std::cell::Ref<$ty>> {
			match ::std::cell::Ref::filter_map(self.data.borrow(), |x| x.$ident.get(name)) {
				Ok(x) => Some(x),
				Err(_) => None,
			}
		}

		pub fn $nmut(&self, name: &str) -> Option<::std::cell::RefMut<$ty>> {
			match ::std::cell::RefMut::filter_map(self.data.borrow_mut(), |x| {
				x.$ident.get_mut(name)
			}) {
				Ok(x) => Some(x),
				Err(_) => None,
			}
		}

		pub fn $nhas(&self, name: &str) -> bool {
			self.data.borrow().$ident.contains_key(name)
		}

		pub fn $nadd(&self, name: &str, thing: $ty) {
			self.data
				.borrow_mut()
				.$ident
				.insert(name.to_string(), thing);
		}
	};
}

impl ResolvedScope {
	get_datum!(get_type get_type_mut has_type insert_type => types (ResolvedMadeType));
	get_datum!(get_type_span get_type_span_mut has_type_span insert_type_span => type_spans (Span));
	get_datum!(get_var get_var_mut has_var insert_var => vars (ResolvedVar));
	get_datum!(get_var_span get_var_span_mut has_var_span insert_var_span => var_spans (Span));
	get_datum!(get_func get_func_mut has_func insert_func => funcs (ResolvedFunc));
	get_datum!(get_func_span get_func_span_mut has_func_span insert_func_span => func_spans (Span));

	pub fn resolve_func_signature(
		&self,
		func: HoistedFuncSignature,
		context: Context,
	) -> ResolvedFuncSignature {
		ResolvedFuncSignature {
			generics: func.generics,
			arg_tys: func
				.arg_tys
				.iter()
				.map(|x| self.resolve_ty(x.clone(), context.clone()))
				.collect(),
			return_ty: self.resolve_ty(func.return_ty, context),
		}
	}

	pub fn resolve_ty_signature(
		&self,
		ty: MadeTypeSignature,
		context: Context,
	) -> ResolvedMadeTypeSignature {
		ResolvedMadeTypeSignature {
			generics: ty.generics,
			fields: ty
				.fields
				.iter()
				.map(|(name, x)| (name.clone(), self.resolve_ty(x.clone(), context.clone())))
				.collect(),
			funcs: ty
				.funcs
				.iter()
				.map(|(name, x)| {
					(
						name.clone(),
						self.resolve_func_signature(x.clone(), context.clone()),
					)
				})
				.collect(),
		}
	}

	pub fn check_type(&self, ty: ResolvedType, context: Context) {
		match ty {
			ResolvedType::BareType(span, x) => {
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
				} else {
					let generics;
					let decl_span;
					let mut found = true;
					if let Some(ty) = self.get_type(&x.ident.to_string()) {
						generics = ty.generics.clone();
						decl_span = self.get_type_span(&x.ident.to_string()).unwrap().clone();
					} else if let Some(ty) = self.old_hoisted.get_type(&x.ident.to_string()) {
						let signature = self.resolve_ty_signature(ty.clone(), context.clone());
						generics = signature.generics.clone();
						decl_span = self
							.old_hoisted
							.get_type_span(&x.ident.to_string())
							.unwrap()
							.clone();
					} else {
						generics = vec![];
						decl_span = x.ident.span();
						found = false;
						add_diagnostic(
							Diagnostic::error()
								.with_message("tried to use undeclared type")
								.with_labels(vec![Label::primary(span.file_id, span.range())]),
						);
					}
					if generics.len() != x.generics.len() && found {
						add_diagnostic(
							Diagnostic::error()
								.with_message(match x.generics.len().cmp(&generics.len()) {
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
				}
				for generic in x.generics {
					self.check_type(generic, context.clone());
				}
			}
			ResolvedType::Ref(_, x, _) | ResolvedType::Optional(_, x) => {
				self.check_type(*x, context.clone())
			}
			ResolvedType::Array(_, x, _size) => {
				self.check_type(*x, context.clone());
			}
			ResolvedType::Inferred(_) => {}
			// we might have to remove inferred in some cases because type inference hehehe
			ResolvedType::Builtin(_, _) => {}
		}
	}

	pub fn add_var(
		&self,
		span: Span,
		ty_ident: ResolvedTypedIdent,
		is_mut: bool,
		value: Option<Option<ResolvedExpr>>,
	) {
		// TODO: do something with duplicate idents. perhaps mangle them in codegen
		if ty_ident.ident.is_discarded() {
			return;
		}
		let name = ty_ident.ident_str();
		let ty = ty_ident.ty;
		self.insert_var(
			&name.clone(),
			ResolvedVar {
				name: name.clone(),
				ty,
				is_mut,
				value,
			},
		);
		self.insert_var_span(&name, span);
	}

	pub fn add_type(&self, span: Span, name: String, ty: ResolvedMadeType) {
		self.insert_type(&name.clone(), ty);
		self.insert_type_span(&name, span);
	}

	pub fn set_var(&self, ident: Ident, resolved_expr: ResolvedExpr) {
		if ident.is_discarded() {
			return;
		}
		if let Some(mut x) = self.get_var_mut(&ident.to_string()) {
			if !x.is_mut {
				let span = ident.span() + resolved_expr.span();
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
			// TODO: check whether this even works
			x.value = Some(Some(resolved_expr));
		} else {
			let span = ident.span() + resolved_expr.span();
			add_diagnostic(
				Diagnostic::error()
					.with_message("tried to set non-existing symbol")
					.with_labels(vec![Label::primary(span.file_id, span.range())]),
			);
		}
	}

	pub fn add_func(&self, name: String, func: ResolvedFunc) {
		let decl_span = func.decl_span.clone();
		self.insert_func(&name.clone(), func);
		self.insert_func_span(&name, decl_span);
	}

	pub fn check_ident_exists(&self, ident: Ident) {
		let vars_has = self.has_var(&ident.to_string());
		let funcs_has = self.has_func(&ident.to_string());
		let hoisted_vars_has = self.old_hoisted.has_var(&ident.to_string());
		let hoisted_funcs_has = self.old_hoisted.has_func(&ident.to_string());
		let hoisted_has = hoisted_vars_has || hoisted_funcs_has;
		if !vars_has && !funcs_has && !hoisted_has {
			if self.old_hoisted.has_func(&ident.to_string())
				|| self.old_hoisted.has_var(&ident.to_string())
			{
				return;
			}
			let span = ident.span();
			add_diagnostic(
				Diagnostic::error()
					.with_message("tried to access non-existing symbol")
					.with_labels(vec![Label::primary(span.file_id, span.range())
						.with_message("couldn't find symbol in current scope")]),
			)
		}
	}

	// TODO: don't own the expr, just ref it!!!
	pub fn check_expr(&self, expr: ResolvedExpr, context: Context) {
		match expr {
			ResolvedExpr::CharLiteral(_, _) => {}
			ResolvedExpr::StringLiteral(_, _) => {}
			ResolvedExpr::NumberLiteral(_, _) => {}
			ResolvedExpr::Identifier(span, ident) => {
				self.check_ident_exists(ident.clone());
				if let Some(var) = self.get_var(&ident.to_string()) {
					if var.value.is_none() {
						let mut diagnostic = Diagnostic::error()
							.with_message("accessed variable's value before giving it one")
							.with_labels(vec![Label::primary(span.file_id, span.range())]);
						if let Some(declspan) = self.get_var_span(&ident.to_string()) {
							diagnostic.labels.push(
								Label::secondary(declspan.file_id, declspan.range())
									.with_message("original declaration here"),
							)
						}
						add_diagnostic(diagnostic);
					}
				}
			}
			ResolvedExpr::BinaryOp(_, lhs, _op, rhs) => {
				self.check_expr(*lhs.clone(), context.clone());
				self.check_expr(*rhs.clone(), context.clone());
				// this may seem useless but it checks for types in binary ops
				self.get_expr_ty(*lhs, context.clone());
				self.get_expr_ty(*rhs, context);
			}
			ResolvedExpr::UnaryOp(_, _op, val) => {
				self.check_expr(*val, context);
			}
			ResolvedExpr::Lambda(_, _func) => {
				// TODO: maybe infer arg types
			}
			ResolvedExpr::Call(span, func_expr, mut generics, args) => {
				self.check_expr(*func_expr.clone(), context.clone());
				let func_expr_span = func_expr.span();
				let (func, decl_span) = if let ResolvedExpr::Identifier(_, name) = *func_expr {
					self.check_ident_exists(name.clone());
					let Some(func) = self.get_func(&name.to_string()) else { return; };
					(
						// TODO: who cloned this
						func.clone(),
						self.get_func_span(&name.to_string())
							.map(|x| x.clone())
							.unwrap_or(func_expr_span),
					)
				} else if let ResolvedExpr::Lambda(span, func) = *func_expr {
					// TODO: add args to inherit scope
					(func, span)
				} else {
					let span = func_expr.span();
					let ty = self.get_expr_ty(*func_expr, context);
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
				if func.attribs.is_unsafe && context != Context::Unsafe {
					add_diagnostic(
						Diagnostic::error()
							.with_message("calling unsafe function from non-unsafe context")
							.with_labels(vec![
								Label::primary(span.file_id, span.range()),
								Label::secondary(decl_span.file_id, decl_span.range())
									.with_message("original declaration here"),
							]),
					)
				}
				if !func.generics.is_empty() {
					if let Some(generics) = generics.clone() {
						if generics.len() != func.generics.len() {
							add_diagnostic(
								Diagnostic::error()
									.with_message(match generics.len().cmp(&func.generics.len()) {
										Ordering::Less => "not enough generics in function call",
										Ordering::Greater => "too many generics in function call",
										Ordering::Equal => unreachable!(),
									})
									.with_labels(vec![
										Label::primary(span.file_id, span.range()),
										Label::secondary(decl_span.file_id, decl_span.range())
											.with_message("original declaration here"),
									]),
							);
						}
					} else {
						generics = Some(vec![
							ResolvedType::Inferred(span.clone());
							func.generics.len()
						])
					}
				}
				let mut arg_types = Vec::new();
				for arg in func.args {
					arg_types.push(arg.ty.replace_generics(
						span.clone(),
						func.generics.clone(),
						generics.clone(),
					));
				}
				for (i, arg) in args.iter().enumerate() {
					self.check_expr(arg.clone(), context.clone());
					let arg_ty = self.get_expr_ty(arg.clone(), context.clone());
					let expected_ty = arg_types[i].clone();
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
			// we check this in the get type function
			ResolvedExpr::Dot(..) => {}
		}
	}

	// TODO: be lazier with int literals and automatically cast them in Stmt::Create
	pub fn get_expr_ty(&self, expr: ResolvedExpr, context: Context) -> ResolvedType {
		match expr {
			ResolvedExpr::CharLiteral(span, _) => builtin!(span, Char),
			ResolvedExpr::StringLiteral(span, _) => builtin!(span, String),
			ResolvedExpr::NumberLiteral(span, literal) => {
				ResolvedType::Builtin(span, literal.as_ty())
			}
			ResolvedExpr::Identifier(span, ident) => self
				.get_var(&ident.to_string())
				.map(|x| x.ty.clone())
				.unwrap_or_else(|| {
					self.old_hoisted
						.get_var(&ident.to_string())
						.map(|x| self.resolve_ty(x.clone(), context))
						.unwrap_or_else(|| builtin!(span, Error))
				}),
			ResolvedExpr::BinaryOp(span, lhs, op, rhs) => {
				let lhs_span = lhs.span();
				let rhs_span = rhs.span();
				let lhs_ty = self.get_expr_ty(*lhs, context.clone());
				let rhs_ty = self.get_expr_ty(*rhs, context);
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
				// TODO: overload with defined operator in case of operator existing
				// TODO: check if operator is defined
				match op {
					Operator::Question | Operator::Bang | Operator::Amp => {
						panic!("invalid bin op in resolve")
					}
					Operator::And
					| Operator::Or
					| Operator::Eq
					| Operator::Ne
					| Operator::Lt
					| Operator::Gt
					| Operator::Le
					| Operator::Ge => builtin!(span, Bool),
					Operator::Neg | Operator::Star | Operator::Plus | Operator::Div => lhs_ty,
				}
			}
			// TODO: overload with defined operator in case of operator existing
			// TODO: &x, *x => ref + deref. these would return non-reffed or reffed tys.
			ResolvedExpr::UnaryOp(_span, op, val) => {
				let ty = self.get_expr_ty(*val, context);
				match op {
					Operator::Question => todo!(), // optional chaining or try, idk
					Operator::Bang => todo!(),     // maybe could be unwrap
					Operator::Star => todo!(),     // dereference
					Operator::Amp => todo!(),      // reference
					Operator::Neg => ty,           // negation
					Operator::And
					| Operator::Or
					| Operator::Eq
					| Operator::Ne
					| Operator::Lt
					| Operator::Gt
					| Operator::Le
					| Operator::Ge
					| Operator::Plus
					| Operator::Div => {
						panic!("invalid unary operator in resolution (parser should've caught it)")
					}
				}
			}
			ResolvedExpr::Lambda(span, _func) => {
				// TODO: resolve lambda
				builtin!(span, Error)
			}
			ResolvedExpr::Call(span, callee, generics, _args) => {
				match *callee {
					ResolvedExpr::Identifier(_, ref i) => match self.get_func(&i.to_string()) {
						Some(x) => {
							x.return_ty
								.clone()
								.replace_generics(span, x.generics.clone(), generics)
						}
						None => match self.old_hoisted.get_func(&i.to_string()) {
							Some(x) => self
								.resolve_ty(x.return_ty.clone(), context)
								.replace_generics(span, x.generics.clone(), generics),
							None => {
								let ty = self.get_expr_ty(*callee, context);
								add_diagnostic(
									Diagnostic::error()
										.with_message("tried to call non-function")
										.with_labels(vec![Label::primary(
											span.file_id,
											span.range(),
										)
										.with_message(format!("expected function, found {ty}"))]),
								);
								builtin!(span, Error)
							}
						},
					},
					ResolvedExpr::Lambda(_, f) => {
						f.return_ty.replace_generics(span, f.generics, generics)
					}
					// TODO: func signature builtin type. so we'd do self.get_expr_ty(expr) then
					// simply force it to be a func and get its return ty
					ResolvedExpr::Call(_, _, _, _) => todo!(),
					_ => builtin!(span, Error),
				}
			}
			ResolvedExpr::Dot(span, lhs, rhs) => {
				let lhs_span = lhs.span();
				let lhs_ty = self.get_expr_ty(*lhs, context.clone());
				let Some((generic_names, fields, funcs)) = lhs_ty.get_underlying(self, context) else { return builtin!(span, Error); };
				let rhs_span = rhs.span();
				let (name, func_stuff) = match *rhs {
					ResolvedExpr::Identifier(_, x) => (x.to_string(), None),
					ResolvedExpr::Call(_, x, generics, args) => match *x {
						ResolvedExpr::Identifier(_, x) => (x.to_string(), Some((generics, args))),
						_ => unreachable!(), // TODO: disallow in expr parser
					},
					_ => unreachable!(),
				};
				let generic_values = lhs_ty.clone().find_top_generics();
				let non_existing_diag = |what: &str| {
					add_diagnostic(
						Diagnostic::error()
							.with_message(format!("tried to access non-existing {what}"))
							.with_labels(vec![
								Label::primary(rhs_span.file_id, rhs_span.range())
									.with_message("invalid access"),
								Label::secondary(lhs_span.file_id, lhs_span.range())
									.with_message(format!("accessing type {lhs_ty}")),
							]),
					);
					builtin!(span.clone(), Error)
				};
				match func_stuff {
					None => {
						let Some(field) = fields.get(&name) else { return non_existing_diag("member"); };
						field
							.clone()
							.replace_generics(span, generic_names, generic_values)
					}
					Some((generics, _args)) => {
						let Some(func) = funcs.get(&name) else { return non_existing_diag("method"); };
						func.return_ty
							.clone()
							.replace_generics(span.clone(), generic_names, generic_values)
							.replace_generics(span, func.generics.clone(), generics)
					}
				}
			}
		}
	}

	pub fn resolve_func(
		&self,
		func_span: Span,
		func: HoistedFunc,
		context: Context,
	) -> ResolvedFunc {
		let frs = ResolvedScope {
			data: RefCell::new(InheritableData::default()),
			stmts: vec![],
			old_hoisted: func.body.clone(),
			span: func.decl_span.clone(),
		};
		let mut generics = Vec::new();
		for generic in &func.generics {
			// TODO: deal with discarded generics
			generics.push(generic.clone());
			let name = generic.to_string();
			check_case(generic.span(), generic.to_string(), Case::PascalCase);
			frs.add_type(
				generic.span(),
				name.clone(),
				ResolvedMadeType {
					name,
					generics: vec![],
					fields: HashMap::new(), // NOTE: potentially in the future we will change this
					funcs: HashMap::new(),
					body: None,
					is_generic: true,
				},
			);
		}
		let resolved_return_ty = self.resolve_ty(func.return_ty, context.clone());
		frs.check_type(resolved_return_ty.clone(), context.clone());
		for arg in &func.args {
			// TODO: deal with discarded args
			frs.check_type(
				self.resolve_ty(arg.ty.clone(), context.clone()),
				context.clone(),
			);
			check_case(arg.ident.span(), arg.ident.to_string(), Case::SnakeCase);
			frs.add_var(
				arg.span.clone(),
				self.resolve_ty_ident(arg.clone(), context.clone()),
				false, // TODO: mut args
				Some(None),
			);
		}
		// TODO: use resolved scope
		let mut return_ty = resolved_return_ty;
		let body = if return_ty.is_inferred() {
			let ((scope, ty), _) = resolve(func.body, Context::Func, None);
			return_ty = ty;
			scope
		} else {
			resolve(
				func.body,
				Context::Func,
				Some((func_span.clone(), return_ty.span(), return_ty.clone())),
			)
			.0
			 .0
		};
		let attribs = func.attribs.clone();
		let mut args = vec![];
		for arg in func.args {
			args.push(ResolvedTypedIdent {
				span: arg.span,
				ty: self.resolve_ty(arg.ty, context.clone()),
				ident: arg.ident,
			});
		}
		ResolvedFunc {
			span: func_span,
			generics,
			args,
			return_ty,
			body,
			attribs,
			decl_span: func.decl_span,
		}
	}

	pub fn resolve_expr(&self, expr: HoistedExpr, context: Context) -> ResolvedExpr {
		let box_res = |x: Box<HoistedExpr>| Box::new(self.resolve_expr(*x, context.clone()));
		match expr {
			Expr::CharLiteral(s, v) => ResolvedExpr::CharLiteral(s, v),
			Expr::StringLiteral(s, v) => ResolvedExpr::StringLiteral(s, v),
			Expr::NumberLiteral(s, v) => ResolvedExpr::NumberLiteral(s, v),
			Expr::Identifier(s, v) => ResolvedExpr::Identifier(s, v),
			Expr::BinaryOp(s, l, o, r) => ResolvedExpr::BinaryOp(s, box_res(l), o, box_res(r)),
			Expr::UnaryOp(s, o, v) => ResolvedExpr::UnaryOp(s, o, box_res(v)),
			// TODO: better lambda span
			Expr::Lambda(s, _f) => {
				// TODO: this is horrible
				// TODO: make HoistedExpr with the new cool system :D
				#[allow(unreachable_code)]
				ResolvedExpr::Lambda(
					s,
					todo!(), // self.resolve_func("~".into(), s, f.hoist(None), context),
				)
			}
			Expr::Call(s, c, g, a) => ResolvedExpr::Call(
				s,
				Box::new(self.resolve_expr(*c, context.clone())),
				g.map(|gen| {
					gen.iter()
						.map(|x| self.resolve_ty(x.clone(), context.clone()))
						.collect()
				}),
				a.iter()
					.map(|x| self.resolve_expr(x.clone(), context.clone()))
					.collect(),
			),
			Expr::Dot(s, l, r) => ResolvedExpr::Dot(s, box_res(l), box_res(r)),
		}
	}

	pub fn resolve_and_check_expr(&self, expr: HoistedExpr, context: Context) -> ResolvedExpr {
		let resolved_expr = self.resolve_expr(expr, context.clone());
		self.check_expr(resolved_expr.clone(), context);
		resolved_expr
	}

	pub fn examine_expr(
		&self,
		expr: HoistedExpr,
		context: Context,
	) -> (ResolvedExpr, ResolvedType) {
		let resolved_expr = self.resolve_and_check_expr(expr, context.clone());
		let ty = self.get_expr_ty(resolved_expr.clone(), context);
		(resolved_expr, ty)
	}

	fn resolve_bare_type(&self, bare_type: HoistedBareType, context: Context) -> ResolvedBareType {
		ResolvedBareType {
			ident: bare_type.ident,
			generics: bare_type
				.generics
				.iter()
				.map(|x| self.resolve_ty(x.clone(), context.clone()))
				.collect(),
		}
	}

	pub fn resolve_ty(&self, ty: HoistedType, context: Context) -> ResolvedType {
		match ty {
			Type::BareType(a, b) => ResolvedType::BareType(a, self.resolve_bare_type(b, context)),
			Type::Builtin(a, b) => ResolvedType::Builtin(a, b),
			Type::Array(a, b, c) => ResolvedType::Array(
				a,
				Box::new(self.resolve_ty(*b, context.clone())),
				c.map(|x| Box::new(self.resolve_and_check_expr(*x, context))),
			),
			Type::Ref(a, b, c) => ResolvedType::Ref(a, Box::new(self.resolve_ty(*b, context)), c),
			Type::Optional(a, b) => {
				ResolvedType::Optional(a, Box::new(self.resolve_ty(*b, context)))
			}
			Type::Inferred(s) => ResolvedType::Inferred(s),
		}
	}

	pub fn resolve_ty_ident(
		&self,
		ty_ident: HoistedTypedIdent,
		context: Context,
	) -> ResolvedTypedIdent {
		ResolvedTypedIdent {
			span: ty_ident.span,
			ty: self.resolve_ty(ty_ident.ty, context),
			ident: ty_ident.ident,
		}
	}
}

impl ResolvedType {
	pub fn find_top_generics(self) -> Option<Vec<ResolvedType>> {
		match self {
			Self::BareType(_, x) => Some(x.generics),
			Self::Builtin(..) => None,
			Self::Array(_, x, _) => x.find_top_generics(),
			Self::Ref(_, x, _) => x.find_top_generics(),
			Self::Optional(_, x) => x.find_top_generics(),
			Self::Inferred(..) => None,
		}
	}

	pub fn replace_generic(self, name: Ident, ty: Self) -> Self {
		match self {
			Self::BareType(span, x) => {
				if x.ident.to_string() == name.to_string() {
					ty
				} else {
					Self::BareType(span, x)
				}
			}
			Self::Builtin(_, _) => self,
			Self::Array(span, x, l) => Self::Array(span, Box::new(x.replace_generic(name, ty)), l),
			Self::Ref(span, x, is_mut) => {
				Self::Ref(span, Box::new(x.replace_generic(name, ty)), is_mut)
			}
			Self::Optional(span, x) => Self::Optional(span, Box::new(x.replace_generic(name, ty))),
			Self::Inferred(_) => self,
		}
	}

	pub fn replace_generics(
		mut self,
		span: Span,
		names: Vec<Ident>,
		tys: Option<Vec<Self>>,
	) -> Self {
		if let Some(tys) = tys {
			if names.len() != tys.len() {
				return self;
			}
			if !names.is_empty() {
				for (i, generic) in names.iter().enumerate() {
					let Some(curr_generic) = tys.get(i) else { return self; };
					self = self.replace_generic(generic.clone(), curr_generic.clone());
				}
			}
			self
		} else {
			let len = names.len();
			self.replace_generics(
				span.clone(),
				names,
				Some(vec![ResolvedType::Inferred(span); len]),
			)
		}
	}

	pub fn get_underlying(
		&self,
		scope: &ResolvedScope,
		context: Context,
	) -> Option<(
		Vec<Ident>,
		HashMap<String, ResolvedType>,
		HashMap<String, ResolvedFuncSignature>,
	)> {
		match self {
			Self::BareType(span, ty) => {
				if scope.has_type(&ty.ident.to_string()) {
					let ty = scope.get_type(&ty.ident.to_string()).unwrap();
					Some((
						ty.generics.clone(),
						ty.fields.clone(),
						ty.funcs
							.iter()
							.map(|(s, x)| (s.clone(), x.signature()))
							.collect(),
					))
				} else if scope.old_hoisted.has_type(&ty.ident.to_string()) {
					let ty = scope.old_hoisted.get_type(&ty.ident.to_string()).unwrap();
					Some((
						ty.generics.clone(),
						ty.fields
							.iter()
							.map(|(s, x)| (s.clone(), scope.resolve_ty(x.clone(), context.clone())))
							.collect(),
						ty.funcs
							.iter()
							.map(|(s, x)| {
								(
									s.clone(),
									scope.resolve_func_signature(x.clone(), context.clone()),
								)
							})
							.collect(),
					))
				} else {
					add_diagnostic(
						Diagnostic::error()
							.with_message("unknown type")
							.with_labels(vec![Label::primary(span.file_id, span.range())
								.with_message("couldn't find type in scope")]),
					);
					None
				}
			}
			Self::Builtin(..) => todo!(),  // does anyone know a good solution?
			Self::Array(..) => todo!(),    // access Array
			Self::Ref(..) => todo!(),      // access Ref
			Self::Optional(..) => todo!(), // access Optional
			Self::Inferred(span) => {
				add_diagnostic(
					Diagnostic::error()
						.with_message("cannot access fields of non-inferred type")
						.with_labels(vec![Label::primary(span.file_id, span.range())]),
				);
				None
			}
		}
	}

	pub fn fields(&self, scope: &ResolvedScope, context: Context) -> HashMap<String, ResolvedType> {
		match self.get_underlying(scope, context) {
			None => HashMap::new(),
			Some((_, fields, _)) => fields,
		}
	}

	pub fn funcs(
		&self,
		scope: &ResolvedScope,
		context: Context,
	) -> HashMap<String, ResolvedFuncSignature> {
		match self.get_underlying(scope, context) {
			None => HashMap::new(),
			Some((_, _, funcs)) => funcs,
		}
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ResolvedVar {
	pub name: String,
	pub ty: ResolvedType,
	pub is_mut: bool,
	// outer option = is initialized, inner option = is arg
	pub value: Option<Option<ResolvedExpr>>,
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct ResolvedMadeTypeSignature {
	pub generics: Vec<Ident>,
	pub fields: HashMap<String, ResolvedType>,
	pub funcs: HashMap<String, ResolvedFuncSignature>,
}

#[derive(Debug, Clone)]
pub struct ResolvedMadeType {
	pub name: String,
	pub generics: Vec<Ident>, // TODO: ResolvedGeneric
	pub fields: HashMap<String, ResolvedType>,
	pub funcs: HashMap<String, ResolvedFunc>,
	pub body: Option<ResolvedScope>, // if it's a generic no body defines it
	pub is_generic: bool,
}

impl ResolvedMadeType {
	pub fn signature(&self) -> ResolvedMadeTypeSignature {
		ResolvedMadeTypeSignature {
			generics: self.generics.clone(),
			fields: self.fields.clone(),
			funcs: self
				.funcs
				.iter()
				.map(|x| (x.0.clone(), x.1.signature()))
				.collect(),
		}
	}
}

impl PartialEq for ResolvedMadeType {
	fn eq(&self, other: &Self) -> bool {
		self.signature() == other.signature()
	}
}

impl Eq for ResolvedMadeType {}

#[derive(Debug, Display, Clone, PartialEq, Eq)]
pub enum Context {
	#[display(fmt = "top level")]
	TopLevel,
	#[display(fmt = "class")]
	Class,
	#[display(fmt = "function")]
	Func,
	#[display(fmt = "unsafe")]
	Unsafe,
}

fn check_privacy(privacy: Privacy, context: Context) {
	let is_valid = privacy.is_default()
		|| match context {
			Context::TopLevel => privacy.is_export(),
			Context::Class => !privacy.is_export(),
			Context::Func => false,
			Context::Unsafe => false,
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

macro_rules! check_stmt {
	($($v:ident => $($ctx:ident)*;)*) => {
		fn check_stmt(stmt: &HoistedStmt, context: &Context) {
			match stmt {
				$(
					HoistedStmt::$v(..) => {
						if $(*context != Context::$ctx)&&* {
							add_diagnostic(
								Diagnostic::error()
									.with_message(format!("invalid {} statement in {context} context", stmt.variant()))
									.with_labels(vec![Label::primary(stmt.span().file_id, stmt.span().range())]),
							);
						}
					}
				)*
				_ => {}
			}
		}
	};
}

check_stmt!(
	Create => TopLevel Class Func Unsafe;
	Declare => TopLevel Class Func Unsafe;
	Set => TopLevel Func Unsafe;
	Return => Func;
	Class => TopLevel Func; // NOTE: maybe Class too?
	Import => TopLevel Unsafe;
	BareExpr => Func Unsafe;
	Unsafe => Func;
	Cpp => Unsafe;
);

pub fn resolve(
	scope: HoistedScope,
	context: Context,
	expected_func_ty: Option<(Span, Span, ResolvedType)>,
) -> ((ResolvedScope, ResolvedType), Vec<Diagnostic<usize>>) {
	let span = scope.span.clone();
	let stmts = scope.stmts.borrow().clone();
	let old_hoisted = scope;
	let mut resolved_scope = ResolvedScope {
		data: RefCell::new(InheritableData::default()),
		stmts: vec![],
		old_hoisted,
		span: span.clone(),
	};
	let mut return_ty = builtin!(span, Void);
	let mut return_span = None;
	for stmt in stmts {
		check_stmt(&stmt, &context);
		match stmt {
			HoistedStmt::Create(span, privacy, ty_ident, is_mut, expr) => {
				check_privacy(privacy.clone(), context.clone());
				let mut ty_ident = resolved_scope.resolve_ty_ident(ty_ident, context.clone());
				check_case(
					ty_ident.ident.span(),
					ty_ident.ident.to_string(),
					Case::SnakeCase,
				);
				resolved_scope.check_type(ty_ident.ty.clone(), context.clone());
				let rhs_span = expr.span();
				let (resolved_expr, rhs) = resolved_scope.examine_expr(expr, context.clone());
				let lhs = ty_ident.ty.clone();
				let lhs_span = ty_ident.ty.span();
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
				ty_ident.ty = if lhs.is_inferred() { rhs } else { lhs };
				resolved_scope.add_var(
					span.clone(),
					ty_ident.clone(),
					is_mut,
					Some(Some(resolved_expr.clone())),
				);
				if !ty_ident.ident.is_discarded() {
					resolved_scope.stmts.push(ResolvedStmt::Create(
						span,
						privacy,
						ty_ident,
						is_mut,
						resolved_expr,
					));
				} else {
					resolved_scope
						.stmts
						.push(ResolvedStmt::BareExpr(span, resolved_expr));
				}
			}
			HoistedStmt::Declare(span, privacy, ty_ident, is_mut) => {
				// TODO: add error if the type is ~
				check_privacy(privacy.clone(), context.clone());
				if context != Context::Class && !is_mut {
					add_diagnostic(Diagnostic::warning().with_message("non-class immutable declaration").with_labels(vec![Label::primary(span.file_id, span.range())]).with_notes(vec!["you will not be able to use or assign this variable, which is probably not intended".into()]));
				}
				let ty_ident = resolved_scope.resolve_ty_ident(ty_ident, context.clone());
				check_case(
					ty_ident.ident.span(),
					ty_ident.ident.to_string(),
					Case::SnakeCase,
				);
				resolved_scope.check_type(ty_ident.ty.clone(), context.clone());
				resolved_scope.add_var(span.clone(), ty_ident.clone(), is_mut, None);
				resolved_scope
					.stmts
					.push(ResolvedStmt::Declare(span, privacy, ty_ident, is_mut));
			}
			HoistedStmt::Set(span, ident, expr) => {
				resolved_scope.check_ident_exists(ident.clone());
				let rhs_span = expr.span();
				let (resolved_expr, rhs) = resolved_scope.examine_expr(expr, context.clone());
				let lhs_span = ident.span();
				let lhs = resolved_scope.get_expr_ty(
					ResolvedExpr::Identifier(lhs_span.clone(), ident.clone()),
					context.clone(),
				);
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
				resolved_scope.set_var(ident.clone(), resolved_expr.clone());
				resolved_scope
					.stmts
					.push(ResolvedStmt::Set(span, ident, resolved_expr));
			}
			HoistedStmt::Func(_span, privacy, ident, func) => {
				check_privacy(privacy, context.clone());
				check_case(ident.span(), ident.to_string(), Case::SnakeCase);
				let resolved = resolved_scope.resolve_func(ident.span(), func, context.clone());
				// TODO: use another, better span
				resolved_scope.add_func(ident.to_string(), resolved);
			}
			HoistedStmt::Return(span, expr) => {
				let (resolved_expr, ty) = resolved_scope.examine_expr(expr, context.clone());
				return_ty = ty;
				return_span = Some(span.clone());
				resolved_scope
					.stmts
					.push(ResolvedStmt::Return(span, resolved_expr));
			}
			HoistedStmt::Class(_span, privacy, ident, generics, decl_span, body) => {
				check_privacy(privacy, context.clone());
				check_case(ident.span(), ident.to_string(), Case::PascalCase);
				let name = ident.to_string();
				let mut ty = ResolvedMadeType {
					name: name.clone(),
					generics: generics.clone(),
					fields: HashMap::new(), // TODO
					funcs: HashMap::new(),  // TODO
					body: None,
					is_generic: false,
				};
				let scope = resolve(body, Context::Class, None).0 .0;
				ty.body = Some(scope);
				resolved_scope.add_type(decl_span, name, ty);
			}
			HoistedStmt::Import(_span, _glob, _imported) => {}
			HoistedStmt::BareExpr(span, expr) => {
				let (resolved_expr, _) = resolved_scope.examine_expr(expr, context.clone());
				resolved_scope
					.stmts
					.push(ResolvedStmt::BareExpr(span, resolved_expr));
			}
			HoistedStmt::Unsafe(span, scope) => {
				let resolved = resolve(scope, Context::Unsafe, None).0 .0;
				resolved_scope
					.stmts
					.push(ResolvedStmt::Unsafe(span, resolved));
			}
			HoistedStmt::Cpp(span, code) => {
				resolved_scope.stmts.push(ResolvedStmt::Cpp(span, code));
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
		((resolved_scope.clone(), return_ty), diagnostics.clone())
	} else {
		((resolved_scope.clone(), return_ty), vec![])
	}
}
