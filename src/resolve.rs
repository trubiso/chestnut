use crate::{
	lexer::{NumberLiteral, Operator},
	parser::types::{
		join_comma, BareType, BuiltinType, Expr, Func, FuncAttribs, Ident, Privacy, Scope, Stmt,
		Type, TypedIdent,
	},
	span::Span,
};
use codespan_reporting::diagnostic::{Diagnostic, Label};
use derive_more::Display;
use lazy_static::lazy_static;
use std::{cmp::Ordering, collections::HashMap, fmt, sync::Mutex};

use self::case::{check_case, Case};

pub mod case;

// TODO: FuncSignature (only types)
// TODO: ClassSignature (only types)

#[derive(Debug, Display, Clone, PartialEq, Eq)]
#[display(fmt = "{ty} {ident}")]
pub struct ResolvedTypedIdent {
	pub span: Span,
	pub ty: ResolvedType,
	pub ident: Ident,
}

impl ResolvedTypedIdent {
	pub fn ident_str(&self) -> String {
		self.ident.to_string()
	}
}

#[derive(Debug, Display, Clone, PartialEq, Eq)]
pub enum ResolvedStmt {
	#[display(fmt = "{_1} {_2} = {_3};")]
	Create(Span, Privacy, ResolvedTypedIdent, ResolvedExpr),
	#[display(fmt = "{_1} {_2};")]
	Declare(Span, Privacy, ResolvedTypedIdent),
	#[display(fmt = "{_1} = {_2};")]
	Set(Span, Ident, ResolvedExpr),
	#[display(fmt = "return {_1};")]
	Return(Span, ResolvedExpr),
	#[display(fmt = "{_1};")]
	BareExpr(Span, ResolvedExpr),
	#[display(fmt = "unsafe {{{_1}}};")]
	Unsafe(Span, ResolvedScope),
	#[display(fmt = r#"cpp {_1};"#)]
	Cpp(Span, String),
}

macro_rules! builtin {
	($s:expr, $v:ident) => {
		ResolvedType::Builtin($s, BuiltinType::$v)
	};
}

impl ResolvedStmt {
	pub fn span(&self) -> Span {
		match self {
			Self::Create(x, _, _, _)
			| Self::Declare(x, _, _)
			| Self::Set(x, _, _)
			| Self::Return(x, _)
			| Self::BareExpr(x, _)
			| Self::Unsafe(x, _)
			| Self::Cpp(x, _) => x.clone(),
		}
	}
}

#[derive(Debug, Clone)]
pub enum ResolvedExpr {
	CharLiteral(Span, String),
	StringLiteral(Span, String),
	NumberLiteral(Span, NumberLiteral),
	Identifier(Span, Ident),
	BinaryOp(Span, Box<ResolvedExpr>, Operator, Box<ResolvedExpr>),
	UnaryOp(Span, Operator, Box<ResolvedExpr>),
	Lambda(Span, ResolvedFunc),
	Call(
		Span,
		Box<ResolvedExpr>,
		Option<Vec<ResolvedType>>,
		Vec<ResolvedExpr>,
	),
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
		)
	}
}

impl Eq for ResolvedExpr {}

impl fmt::Display for ResolvedExpr {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			ResolvedExpr::CharLiteral(_, x) => f.write_fmt(format_args!("{x}")),
			ResolvedExpr::StringLiteral(_, x) => f.write_fmt(format_args!("{x}")),
			ResolvedExpr::NumberLiteral(_, x) => f.write_fmt(format_args!("{x}")),
			ResolvedExpr::Identifier(_, x) => f.write_fmt(format_args!("{x}")),
			ResolvedExpr::BinaryOp(_, lhs, op, rhs) => {
				f.write_fmt(format_args!("({lhs} {op} {rhs})"))
			}
			ResolvedExpr::UnaryOp(_, op, expr) => f.write_fmt(format_args!("({op}{expr})")),
			ResolvedExpr::Lambda(_, func) => f.write_fmt(format_args!("lambda {func}")),
			ResolvedExpr::Call(_, callee, generics, args) => f.write_fmt(format_args!(
				"{callee}{}({})",
				generics
					.as_ref()
					.map(|g| join_comma(&g)
						.map(|x| format!("<{x}>"))
						.unwrap_or("".to_string()))
					.unwrap_or("".to_string()),
				join_comma(args).unwrap_or("".to_string())
			)),
		}
	}
}

impl ResolvedExpr {
	pub fn span(&self) -> Span {
		match self {
			Self::CharLiteral(x, _)
			| Self::StringLiteral(x, _)
			| Self::NumberLiteral(x, _)
			| Self::Identifier(x, _)
			| Self::BinaryOp(x, _, _, _)
			| Self::UnaryOp(x, _, _)
			| Self::Lambda(x, _)
			| Self::Call(x, _, _, _) => x.clone(),
		}
	}
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct InheritableData {
	pub vars: HashMap<String, ResolvedVar>,
	pub var_spans: HashMap<String, Span>,
	// TODO: top level analyzer
	// TODO: function overloads
	pub funcs: HashMap<String, ResolvedFunc>,
	pub func_spans: HashMap<String, Span>,
	pub types: HashMap<String, ResolvedMadeType>,
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ResolvedScope {
	pub span: Span,
	pub data: InheritableData,
	pub inherit: InheritableData,
	pub stmts: Vec<ResolvedStmt>,
}

impl ResolvedScope {
	pub fn new(span: Span) -> Self {
		Self {
			span,
			data: InheritableData::default(),
			inherit: InheritableData::default(),
			stmts: Vec::new(),
		}
	}
}

impl fmt::Display for ResolvedScope {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		for stmt in &self.stmts {
			f.write_fmt(format_args!("{stmt}\n"))?;
		}
		Ok(())
	}
}

impl ResolvedScope {
	pub fn braced(&self) -> String {
		let body = format!("{}", self);
		format!(
			"{{{}}}",
			if self.stmts.is_empty() {
				"".into()
			} else {
				format!("\n{}", {
					let mut x: Vec<_> = body.split('\n').collect();
					x.pop();
					x.iter()
						.map(|x| -> String { format!("\t{}\n", x) })
						.reduce(|acc, b| acc + &b)
						.unwrap()
				})
			},
		)
	}
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
	get_datum!(get_type get_type_mut has_type => types (ResolvedMadeType));
	get_datum!(get_type_span get_type_span_mut has_type_span => type_spans (Span));
	get_datum!(get_var get_var_mut has_var => vars (ResolvedVar));
	get_datum!(get_var_span get_var_span_mut has_var_span => var_spans (Span));
	get_datum!(get_func get_func_mut has_func => funcs (ResolvedFunc));
	get_datum!(get_func_span get_func_span_mut has_func_span => func_spans (Span));

	pub fn check_type(&self, ty: ResolvedType) {
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
			ResolvedType::Ref(_, x, _) | ResolvedType::Optional(_, x) => {
				self.check_type(*x)
			}
			ResolvedType::Array(_, x, _size) => {
				self.check_type(*x);
			}
			ResolvedType::Inferred(_) => {}
			// we might have to remove inferred in some cases because type inference hehehe
			ResolvedType::Builtin(_, _) => {}
		}
	}

	pub fn add_var(
		&mut self,
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
		self.data.vars.insert(
			name.clone(),
			ResolvedVar {
				name: name.clone(),
				ty,
				is_mut,
				value,
			},
		);
		self.data.var_spans.insert(name, span);
	}

	pub fn add_type(&mut self, span: Span, name: String, ty: ResolvedMadeType) {
		self.data.types.insert(name.clone(), ty);
		self.data.type_spans.insert(name, span);
	}

	pub fn set_var(&mut self, ident: Ident, resolved_expr: ResolvedExpr) {
		if ident.is_discarded() {
			return;
		}
		if let Some(x) = self.get_var_mut(&ident.to_string()) {
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

	pub fn add_func(&mut self, name: String, func: ResolvedFunc) {
		let decl_span = func.decl_span.clone();
		self.data.funcs.insert(name.clone(), func);
		self.data.func_spans.insert(name, decl_span);
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
				self.check_expr(*rhs.clone(), context);
				// this may seem useless but it checks for types in binary ops
				self.get_expr_ty(*lhs);
				self.get_expr_ty(*rhs);
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
						func.clone(),
						self.get_func_span(&name.to_string())
							.cloned()
							.unwrap_or(func_expr_span),
					)
				} else if let ResolvedExpr::Lambda(span, func) = *func_expr {
					// TODO: add args to inherit scope
					(func, span)
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
					let arg_ty = self.get_expr_ty(arg.clone());
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
		}
	}

	// TODO: be lazier with int literals and automatically cast them in Stmt::Create
	pub fn get_expr_ty(&self, expr: ResolvedExpr) -> ResolvedType {
		match expr {
			ResolvedExpr::CharLiteral(span, _) => builtin!(span, Char),
			ResolvedExpr::StringLiteral(span, _) => builtin!(span, String),
			ResolvedExpr::NumberLiteral(span, literal) => {
				ResolvedType::Builtin(span, literal.as_ty())
			}
			ResolvedExpr::Identifier(span, ident) => self
				.get_var(&ident.to_string())
				.map(|x| x.ty.clone())
				.unwrap_or_else(|| builtin!(span, Error)),
			ResolvedExpr::BinaryOp(span, lhs, op, rhs) => {
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
				let ty = self.get_expr_ty(*val);
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
					ResolvedExpr::Identifier(_, i) => match self.get_func(&i.to_string()) {
						Some(x) => {
							x.return_ty
								.clone()
								.replace_generics(span, x.generics.clone(), generics)
						}
						None => builtin!(span, Error),
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
		}
	}

	pub fn resolve_func(
		&self,
		name: String,
		func_span: Span,
		func: Func,
		context: Context,
	) -> ResolvedFunc {
		let mut frs = self.clone();
		let mut generics = Vec::new();
		for generic in &func.generics {
			// TODO: deal with discarded generics
			generics.push(generic.to_string());
			let name = generic.to_string();
			check_case(generic.span(), generic.to_string(), Case::PascalCase);
			frs.add_type(
				generic.span(),
				name.clone(),
				ResolvedMadeType {
					name,
					generic_count: 0,
					fields: HashMap::new(), // NOTE: potentially in the future we will change this
					funcs: HashMap::new(),
					body: None,
					is_generic: true,
				},
			);
		}
		let resolved_return_ty = self.resolve_ty(func.return_ty, context.clone());
		frs.check_type(resolved_return_ty.clone());
		for arg in &func.args {
			// TODO: deal with discarded args
			frs.check_type(self.resolve_ty(arg.ty.clone(), context.clone()));
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
			match resolve(func.body.clone(), Context::Func, Some(frs), None) {
				Ok((scope, ty)) => {
					return_ty = ty;
					scope
				}
				Err(_) => ResolvedScope::new(func.body.span),
			}
		} else {
			match resolve(
				func.body.clone(),
				Context::Func,
				Some(frs),
				Some((func_span, return_ty.span(), return_ty.clone())),
			) {
				Ok((scope, _)) => scope,
				Err(_) => ResolvedScope::new(func.body.span),
			}
		};
		let attribs = func.attribs.clone();
		let mut args = vec![];
		for arg in func.args {
			let name = arg.ident_str();
			args.push(ResolvedArg {
				span: arg.span,
				name,
				ty: self.resolve_ty(arg.ty, context.clone()),
			});
		}
		ResolvedFunc {
			name,
			generics,
			args,
			return_ty,
			body,
			attribs,
			decl_span: func.decl_span,
		}
	}

	pub fn resolve_expr(&self, expr: Expr, context: Context) -> ResolvedExpr {
		match expr {
			Expr::CharLiteral(s, v) => ResolvedExpr::CharLiteral(s, v),
			Expr::StringLiteral(s, v) => ResolvedExpr::StringLiteral(s, v),
			Expr::NumberLiteral(s, v) => ResolvedExpr::NumberLiteral(s, v),
			Expr::Identifier(s, v) => ResolvedExpr::Identifier(s, v),
			Expr::BinaryOp(s, l, o, r) => ResolvedExpr::BinaryOp(
				s,
				Box::new(self.resolve_expr(*l, context.clone())),
				o,
				Box::new(self.resolve_expr(*r, context)),
			),
			Expr::UnaryOp(s, o, v) => {
				ResolvedExpr::UnaryOp(s, o, Box::new(self.resolve_expr(*v, context)))
			}
			// TODO: better lambda span
			Expr::Lambda(s, f) => {
				ResolvedExpr::Lambda(s.clone(), self.resolve_func("~".into(), s, f, context))
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
			Expr::Error(_) => panic!(),
		}
	}

	pub fn resolve_and_check_expr(&self, expr: Expr, context: Context) -> ResolvedExpr {
		let resolved_expr = self.resolve_expr(expr, context.clone());
		self.check_expr(resolved_expr.clone(), context);
		resolved_expr
	}

	pub fn examine_expr(&self, expr: Expr, context: Context) -> (ResolvedExpr, ResolvedType) {
		let resolved_expr = self.resolve_and_check_expr(expr, context);
		let ty = self.get_expr_ty(resolved_expr.clone());
		(resolved_expr, ty)
	}

	fn resolve_bare_type(&self, bare_type: BareType, context: Context) -> ResolvedBareType {
		ResolvedBareType {
			ident: bare_type.ident,
			generics: bare_type
				.generics
				.iter()
				.map(|x| self.resolve_ty(x.clone(), context.clone()))
				.collect(),
		}
	}

	pub fn resolve_ty(&self, ty: Type, context: Context) -> ResolvedType {
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

	pub fn resolve_ty_ident(&self, ty_ident: TypedIdent, context: Context) -> ResolvedTypedIdent {
		ResolvedTypedIdent {
			span: ty_ident.span,
			ty: self.resolve_ty(ty_ident.ty, context),
			ident: ty_ident.ident,
		}
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ResolvedBareType {
	pub ident: Ident,
	pub generics: Vec<ResolvedType>,
}

impl fmt::Display for ResolvedBareType {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		f.write_fmt(format_args!(
			"{}{}",
			self.ident,
			match join_comma(&self.generics) {
				None => "".to_string(),
				Some(x) => format!("<{x}>"),
			},
		))
	}
}

#[derive(Debug, Clone)]
pub enum ResolvedType {
	BareType(Span, ResolvedBareType),
	Builtin(Span, BuiltinType),
	Array(Span, Box<ResolvedType>, Option<Box<ResolvedExpr>>),
	Ref(Span, Box<ResolvedType>, bool),
	Optional(Span, Box<ResolvedType>),
	Inferred(Span),
}

impl PartialEq for ResolvedType {
	fn eq(&self, other: &Self) -> bool {
		match self {
			Self::BareType(_, x) => {
				if let Self::BareType(_, y) = other {
					x == y
				} else {
					false
				}
			}
			Self::Builtin(_, x) => {
				if let Self::Builtin(_, y) = other {
					x == y
				} else {
					false
				}
			}
			Self::Array(_, a, b) => {
				if let Self::Array(_, c, d) = other {
					a == c && b == d
				} else {
					false
				}
			}
			Self::Ref(_, x, a) => {
				if let Self::Ref(_, y, b) = other {
					*x == *y && *a == *b
				} else {
					false
				}
			}
			Self::Optional(_, x) => {
				if let Self::Optional(_, y) = other {
					*x == *y
				} else {
					false
				}
			}
			Self::Inferred(_) => matches!(other, Self::Inferred(_)),
		}
	}
}

impl Eq for ResolvedType {}

impl ResolvedType {
	pub fn span(&self) -> Span {
		match self {
			Self::BareType(x, ..)
			| Self::Builtin(x, ..)
			| Self::Array(x, ..)
			| Self::Ref(x, ..)
			| Self::Optional(x, ..)
			| Self::Inferred(x, ..) => x.clone(),
		}
	}

	pub fn is_inferred(&self) -> bool {
		match self {
			Self::Inferred(_) => true,
			Self::BareType(_, _) | Self::Builtin(_, _) => false,
			Self::Array(_, x, _) | Self::Ref(_, x, _) | Self::Optional(_, x) => x.is_inferred(),
		}
	}

	pub fn is_void(&self) -> bool {
		let Self::Builtin(_, x) = self else { return false; };
		*x == BuiltinType::Void
	}

	pub fn replace_generic(self, name: String, ty: ResolvedType) -> Self {
		match self {
			Self::BareType(span, x) => {
				if x.ident.to_string() == name {
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
		names: Vec<String>,
		tys: Option<Vec<ResolvedType>>,
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
}

impl fmt::Display for ResolvedType {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			ResolvedType::BareType(_, x) => f.write_fmt(format_args!("{x}")),
			ResolvedType::Builtin(_, x) => f.write_fmt(format_args!("{x}")),
			ResolvedType::Array(_, x, len) => f.write_fmt(format_args!(
				"{x}{}",
				if let Some(len) = len {
					format!("[{len}]")
				} else {
					"[]".to_string()
				}
			)),
			ResolvedType::Ref(_, x, is_mut) => {
				f.write_fmt(format_args!("{x}{}&", if *is_mut { " mut" } else { "" }))
			}
			ResolvedType::Optional(_, x) => f.write_fmt(format_args!("{x}?")),
			ResolvedType::Inferred(_) => f.write_str("~"),
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ResolvedMadeType {
	pub name: String,
	pub generic_count: usize, // TODO: ResolvedGeneric
	pub fields: HashMap<String, ResolvedMadeType>,
	pub funcs: HashMap<String, ResolvedFunc>,
	pub body: Option<ResolvedScope>, // if it's a generic no body defines it
	pub is_generic: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ResolvedFunc {
	pub name: String,
	// TODO: generic constraints
	pub generics: Vec<String>, // stores names
	pub args: Vec<ResolvedArg>,
	pub return_ty: ResolvedType,
	pub body: ResolvedScope,
	pub attribs: FuncAttribs,
	pub decl_span: Span,
}

impl fmt::Display for ResolvedFunc {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		f.write_fmt(format_args!(
			"({}) {}-> {} {}",
			join_comma(&self.args).unwrap_or("".into()),
			self.attribs,
			self.return_ty,
			self.body.braced(),
		))
	}
}

#[derive(Debug, Display, Clone, PartialEq, Eq)]
#[display(fmt = "{ty} {name}")]
pub struct ResolvedArg {
	pub span: Span,
	pub ty: ResolvedType,
	pub name: String,
}

impl ResolvedArg {
	pub fn as_ty_ident(self) -> ResolvedTypedIdent {
		ResolvedTypedIdent {
			span: self.span.clone(),
			ty: self.ty,
			ident: Ident::Named(self.span, self.name),
		}
	}
}

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

impl Stmt {
	pub fn variant(&self) -> String {
		match self {
			Self::Create(..) => "creation".into(),
			Self::Declare(..) => "declaration".into(),
			Self::Set(..) => "set".into(),
			Self::Func(..) => "function".into(),
			Self::Return(..) => "return".into(),
			Self::Class(..) => "class".into(),
			Self::Import(..) => "import".into(),
			Self::BareExpr(..) => "bare expression".into(),
			Self::Unsafe(..) => "unsafe scope".into(),
			Self::Cpp(..) => "inline c++".into(),
		}
	}
}

macro_rules! check_stmt {
	($($v:ident => $($ctx:ident)*;)*) => {
		fn check_stmt(stmt: &Stmt, context: &Context) {
			match stmt {
				$(
					Stmt::$v(..) => {
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
	scope: Scope,
	context: Context,
	inherit_scope: Option<ResolvedScope>,
	expected_func_ty: Option<(Span, Span, ResolvedType)>,
) -> Result<(ResolvedScope, ResolvedType), Vec<Diagnostic<usize>>> {
	let mut resolved_scope = ResolvedScope::new(scope.span.clone());
	if let Some(scope) = inherit_scope {
		resolved_scope.inherit = scope.data + scope.inherit;
	}
	let mut return_ty = builtin!(scope.span, Void);
	let mut return_span = None;
	for stmt in scope.stmts {
		check_stmt(&stmt, &context);
		match stmt {
			Stmt::Create(span, privacy, ty_ident, is_mut, expr) => {
				check_privacy(privacy.clone(), context.clone());
				let mut ty_ident = resolved_scope.resolve_ty_ident(ty_ident, context.clone());
				check_case(
					ty_ident.ident.span(),
					ty_ident.ident.to_string(),
					Case::SnakeCase,
				);
				resolved_scope.check_type(ty_ident.ty.clone());
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
						resolved_expr,
					));
				} else {
					resolved_scope
						.stmts
						.push(ResolvedStmt::BareExpr(span, resolved_expr));
				}
			}
			Stmt::Declare(span, privacy, ty_ident, is_mut) => {
				// TODO: add error if the type is ~
				check_privacy(privacy.clone(), context.clone());
				let ty_ident = resolved_scope.resolve_ty_ident(ty_ident, context.clone());
				check_case(
					ty_ident.ident.span(),
					ty_ident.ident.to_string(),
					Case::SnakeCase,
				);
				resolved_scope.check_type(ty_ident.ty.clone());
				resolved_scope.add_var(span.clone(), ty_ident.clone(), is_mut, None);
				resolved_scope
					.stmts
					.push(ResolvedStmt::Declare(span, privacy, ty_ident));
			}
			Stmt::Set(span, ident, expr) => {
				resolved_scope.check_ident_exists(ident.clone());
				let rhs_span = expr.span();
				let (resolved_expr, rhs) = resolved_scope.examine_expr(expr, context.clone());
				let lhs_span = ident.span();
				let lhs = resolved_scope
					.get_expr_ty(ResolvedExpr::Identifier(lhs_span.clone(), ident.clone()));
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
			Stmt::Func(_span, privacy, ident, func) => {
				check_privacy(privacy, context.clone());
				check_case(ident.span(), ident.to_string(), Case::SnakeCase);
				let resolved = resolved_scope.resolve_func(
					ident.to_string(),
					ident.span(),
					func,
					context.clone(),
				);
				// TODO: use another, better span
				resolved_scope.add_func(ident.to_string(), resolved);
			}
			Stmt::Return(span, expr) => {
				let (resolved_expr, ty) = resolved_scope.examine_expr(expr, context.clone());
				return_ty = ty;
				return_span = Some(span.clone());
				resolved_scope
					.stmts
					.push(ResolvedStmt::Return(span, resolved_expr));
			}
			Stmt::Class(_span, privacy, ident, generics, decl_span, body) => {
				check_privacy(privacy, context.clone());
				check_case(ident.span(), ident.to_string(), Case::PascalCase);
				let name = ident.to_string();
				let mut crs = resolved_scope.clone();
				let mut ty = ResolvedMadeType {
					name: name.clone(),
					generic_count: generics.len(),
					fields: HashMap::new(), // TODO
					funcs: HashMap::new(),  // TODO
					body: None,
					is_generic: false,
				};
				crs.add_type(decl_span.clone(), name.clone(), ty.clone());
				for generic in generics {
					check_case(generic.span(), generic.to_string(), Case::PascalCase);
					let name = generic.to_string();
					crs.add_type(
						generic.span(),
						name.clone(),
						ResolvedMadeType {
							name,
							generic_count: 0,
							// NOTE: potentially in the future we will change this
							fields: HashMap::new(),
							funcs: HashMap::new(),
							body: None,
							is_generic: true,
						},
					)
				}
				let scope = match resolve(body.clone(), Context::Class, Some(crs), None) {
					Ok((scope, _)) => scope,
					Err(_) => ResolvedScope::new(body.span),
				};
				ty.body = Some(scope);
				resolved_scope.add_type(decl_span, name, ty);
			}
			Stmt::Import(_span, _glob, _imported) => {}
			Stmt::BareExpr(span, expr) => {
				let resolved_expr = resolved_scope.resolve_and_check_expr(expr, context.clone());
				resolved_scope
					.stmts
					.push(ResolvedStmt::BareExpr(span, resolved_expr));
			}
			Stmt::Unsafe(span, scope) => {
				let scope_span = scope.span.clone();
				let resolved =
					match resolve(scope, Context::Unsafe, Some(resolved_scope.clone()), None) {
						Ok((scope, _)) => scope,
						Err(_) => ResolvedScope::new(scope_span),
					};
				resolved_scope
					.stmts
					.push(ResolvedStmt::Unsafe(span, resolved));
			}
			Stmt::Cpp(span, code) => {
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
		Err(diagnostics.clone())
	} else {
		Ok((resolved_scope, return_ty))
	}
}
