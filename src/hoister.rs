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
use std::{
	cell::{RefCell, RefMut, Ref},
	collections::HashMap,
	fmt,
	sync::Mutex,
};

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
pub struct MadeTypeSignature<'a> {
	pub fields: HashMap<String, HoistedType<'a>>,
	pub funcs: HashMap<String, HoistedFuncSignature<'a>>,
}

pub type HoistedType<'a> = Type<HoistedExpr<'a>>;
pub type HoistedTypedIdent<'a> = TypedIdent<HoistedType<'a>>;
pub type HoistedBareType<'a> = BareType<HoistedType<'a>>;
pub type HoistedFunc<'a> = Func<HoistedExpr<'a>, HoistedScope<'a>>;
pub type HoistedFuncSignature<'a> = FuncSignature<HoistedType<'a>>;
pub type HoistedExpr<'a> = Expr<HoistedScope<'a>>;
pub type HoistedStmt<'a> = Stmt<HoistedExpr<'a>, HoistedFunc<'a>, HoistedScope<'a>>;

#[derive(Debug, Default, Clone)]
pub struct HoistedScopeData<'a> {
	pub vars: HashMap<String, HoistedType<'a>>,
	pub var_spans: HashMap<String, Span>,
	pub funcs: HashMap<String, HoistedFuncSignature<'a>>,
	pub func_spans: HashMap<String, Span>,
	pub types: HashMap<String, MadeTypeSignature<'a>>,
	pub type_spans: HashMap<String, Span>,
}

#[derive(Debug, Clone)]
pub struct HoistedScope<'a> {
	pub data: HoistedScopeData<'a>,
	pub stmts: RefCell<Vec<HoistedStmt<'a>>>,
	pub inherit: Option<&'a HoistedScope<'a>>,
	pub span: Span,
}

impl fmt::Display for HoistedScope<'_> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		self.scope_fmt(f)
	}
}

impl<'a> HoistedScope<'a> {
	get_datum!(get_type get_type_mut has_type add_type => types (MadeTypeSignature));
	get_datum!(get_type_span get_type_span_mut has_type_span add_type_span => type_spans (Span));
	get_datum!(get_var get_var_mut has_var add_var => vars (HoistedType));
	get_datum!(get_var_span get_var_span_mut has_var_span add_var_span => var_spans (Span));
	get_datum!(get_func get_func_mut has_func add_func => funcs (HoistedFuncSignature));
	get_datum!(get_func_span get_func_span_mut has_func_span add_func_span => func_spans (Span));

	pub fn stmts_mut(&self) -> RefMut<'a, Vec<HoistedStmt>> {
		self.stmts.borrow_mut()
	}
}

impl<'a> Scope<HoistedExpr<'a>> for HoistedScope<'a> {
	fn stmts(&self) -> &Vec<Stmt<HoistedExpr<'a>, crate::common::Func<HoistedExpr<'a>, Self>, Self>>
	where
		Self: Sized,
	{
		// TODO: fix
		// SAFETY: don't worry about it
		unsafe { &*self.stmts.as_ptr() }
	}
}

impl ParserExpr {
	// NOTE: what should we do here? maybe just move the values to the "hoisted"
	// ones, i don't think we need to do anything. we could even get rid of inherit
	// and save like 100% of this code's .clone()s (or not, lambdas)
	pub fn hoist<'a>(self, inherit: Option<&HoistedScope>) -> HoistedExpr<'a> {
		match self {
			Expr::CharLiteral(a, b) => Expr::CharLiteral(a, b),
			Expr::StringLiteral(a, b) => Expr::StringLiteral(a, b),
			Expr::NumberLiteral(a, b) => Expr::NumberLiteral(a, b),
			Expr::Identifier(a, b) => Expr::Identifier(a, b),
			Expr::BinaryOp(a, b, c, d) => Expr::BinaryOp(
				a,
				Box::new(b.hoist(inherit.clone())),
				c,
				Box::new(d.hoist(inherit)),
			),
			Expr::UnaryOp(a, b, c) => Expr::UnaryOp(a, b, Box::new(c.hoist(inherit))),
			Expr::Lambda(a, b) => Expr::Lambda(a, b.hoist(inherit)),
			Expr::Call(a, b, c, d) => Expr::Call(
				a,
				Box::new(b.hoist(inherit.clone())),
				c.map(|x| x.iter().map(|y| y.clone().hoist(inherit.clone())).collect()),
				d.iter().map(|x| x.clone().hoist(inherit.clone())).collect(),
			),
			Expr::Dot(a, b, c) => Expr::Dot(
				a,
				Box::new(b.hoist(inherit.clone())),
				Box::new(c.hoist(inherit)),
			),
		}
	}
}

impl ParserTypedIdent {
	pub fn hoist<'a>(self, inherit: Option<&HoistedScope>) -> HoistedTypedIdent<'a> {
		HoistedTypedIdent {
			span: self.span,
			ty: self.ty.hoist(inherit),
			ident: self.ident,
		}
	}
}

impl ParserType {
	pub fn hoist<'a>(self, inherit: Option<&HoistedScope>) -> HoistedType<'a> {
		match self {
			Type::BareType(s, t) => Type::BareType(
				s,
				HoistedBareType {
					ident: t.ident,
					generics: t
						.generics
						.iter()
						.map(|x| x.clone().hoist(inherit))
						.collect(),
				},
			),
			Type::Builtin(s, t) => Type::Builtin(s, t),
			Type::Array(s, l, r) => Type::Array(
				s,
				Box::new(l.hoist(inherit)),
				r.map(|x| Box::new(x.hoist(inherit))),
			),
			Type::Ref(s, t, m) => Type::Ref(s, Box::new(t.hoist(inherit)), m),
			Type::Optional(s, t) => Type::Optional(s, Box::new(t.hoist(inherit))),
			Type::Inferred(s) => Type::Inferred(s),
		}
	}
}

impl ParserFunc {
	pub fn hoist<'a>(self, inherit: Option<&'a HoistedScope<'a>>) -> HoistedFunc<'a> {
		HoistedFunc {
			span: self.span,
			return_ty: self.return_ty.hoist(inherit),
			args: self.args.iter().map(|x| x.clone().hoist(inherit)).collect(),
			generics: self.generics,
			body: hoist(self.body, inherit).0,
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

pub fn hoist<'a>(
	scope: ParserScope,
	inherit: Option<&'a HoistedScope<'a>>,
) -> (HoistedScope<'_>, Vec<Diagnostic<usize>>) {
	let mut hoisted = HoistedScope {
		data: HoistedScopeData::default(),
		stmts: RefCell::new(vec![]),
		inherit: inherit,
		span: scope.span,
	};
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
					ty_ident.ty.clone().hoist(Some(&hoisted)),
				);
				hoisted.add_var_span(&ty_ident.ident_str(), span.clone());
				let hoisted_ty_ident = ty_ident.hoist(Some(&hoisted));
				// NOTE: ???????
				let hoisted_expr = expr.hoist(Some(&hoisted));
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
					ty_ident.ty.clone().hoist(Some(&hoisted)),
				);
				hoisted.add_var_span(&ty_ident.ident_str(), span.clone());
				let hoisted_ty_ident = ty_ident.hoist(Some(&hoisted));
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
				let hoisted_expr = expr.hoist(Some(&hoisted));
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
				let hoisted_scope = hoist(body, Some(&hoisted)).0;
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
				let hoisted_scope = hoist(scope, Some(&hoisted)).0;
				hoisted
					.stmts_mut()
					.push(HoistedStmt::Unsafe(span, hoisted_scope));
			}
			// NOTE: do NOT hoist return. why would you return a value _after_ the return keyword???
			Stmt::Return(span, value) => {
				let hoisted_value = value.hoist(Some(&hoisted));
				hoisted
					.stmts_mut()
					.push(HoistedStmt::Return(span, hoisted_value));
			}
			// TODO: properly hoist bare exprs
			Stmt::BareExpr(span, expr) => {
				let hoisted_expr = expr.hoist(Some(&hoisted));
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
				let hoisted_func = func.hoist(Some(&hoisted));
				hoisted
					.stmts_mut()
					.push(HoistedStmt::Func(span, privacy, ident, hoisted_func));
			}
			_ => unreachable!(),
		}
	}
	if !DIAGNOSTICS.lock().unwrap().is_empty() {
		let diagnostics = DIAGNOSTICS.lock().unwrap();
		(hoisted.clone(), diagnostics.clone())
	} else {
		(hoisted.clone(), vec![])
	}
}
