use crate::{
	common::{BareType, Expr, Func, FuncSignature, Scope, ScopeFmt, Stmt, Type, TypedIdent},
	parser::types::{
		Ident, ParserExpr, ParserFunc, ParserScope, ParserStmt, ParserType, ParserTypedIdent, ParserFuncSignature,
	},
	span::Span,
};
use codespan_reporting::diagnostic::{Diagnostic, Label};
use lazy_static::lazy_static;
use std::{
	cell::{Ref, RefCell, RefMut},
	collections::HashMap,
	fmt,
	rc::Rc,
	sync::Mutex,
};

macro_rules! get_datum {
	($nget:ident $nmut:ident $nhas:ident $nadd:ident => $ident:ident ($ty:ty)) => {
		pub fn $nget(&self, name: &str) -> Option<::std::cell::Ref<$ty>> {
			match ::std::cell::Ref::filter_map(self.data.borrow(), |x| x.$ident.get(name)) {
				Ok(x) => Some(x),
				Err(_) => self.inherit.as_ref()?.$nget(name),
			}
		}

		pub fn $nmut(&self, name: &str) -> Option<::std::cell::RefMut<$ty>> {
			match ::std::cell::RefMut::filter_map(self.data.borrow_mut(), |x| {
				x.$ident.get_mut(name)
			}) {
				Ok(x) => Some(x),
				Err(_) => self.inherit.as_ref()?.$nmut(name),
			}
		}

		pub fn $nhas(&self, name: &str) -> bool {
			self.data.borrow().$ident.contains_key(name)
				|| self
					.inherit
					.as_ref()
					.map(|x| x.$nhas(name))
					.unwrap_or(false)
		}

		pub fn $nadd(&self, name: &str, thing: $ty) {
			self.data
				.borrow_mut()
				.$ident
				.insert(name.to_string(), thing);
		}
	};
}

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

// TODO: genericize for hoister and resolver to share common type sig class
// TODO: add field & func decl_spans for better error reporting
#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct MadeTypeSignature {
	pub generics: Vec<Ident>,
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

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct HoistedScopeData {
	pub vars: HashMap<String, HoistedType>,
	pub var_mutabilities: HashMap<String, bool>,
	pub var_spans: HashMap<String, Span>,
	pub funcs: HashMap<String, HoistedFuncSignature>,
	pub func_spans: HashMap<String, Span>,
	pub types: HashMap<String, MadeTypeSignature>,
	pub type_spans: HashMap<String, Span>,
}

// NOTE: PartialEq and Eq are not, in fact, needed, i need to derive them just
// to be able to equate type signatures, even though they don't require the
// equality of hoisted scopes, it's just a hack so rust shuts up lol
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HoistedScope {
	pub data: RefCell<HoistedScopeData>,
	pub stmts: RefCell<Vec<HoistedStmt>>,
	pub inherit: Option<Rc<HoistedScope>>,
	pub span: Span,
}

impl fmt::Display for HoistedScope {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		self.scope_fmt(f)
	}
}

impl HoistedScope {
	get_datum!(get_type get_type_mut has_type add_type => types (MadeTypeSignature));
	get_datum!(get_type_span get_type_span_mut has_type_span add_type_span => type_spans (Span));
	get_datum!(get_var get_var_mut has_var add_var => vars (HoistedType));
	get_datum!(get_var_mutability get_var_mutability_mut has_var_mutability add_var_mutability => var_mutabilities (bool));
	get_datum!(get_var_span get_var_span_mut has_var_span add_var_span => var_spans (Span));
	get_datum!(get_func get_func_mut has_func add_func => funcs (HoistedFuncSignature));
	get_datum!(get_func_span get_func_span_mut has_func_span add_func_span => func_spans (Span));

	pub fn stmts_mut(&self) -> RefMut<Vec<HoistedStmt>> {
		self.stmts.borrow_mut()
	}
}

impl Scope<HoistedExpr> for HoistedScope {
	fn stmts(&self) -> &Vec<Stmt<HoistedExpr, crate::common::Func<HoistedExpr, Self>, Self>>
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
	pub fn hoist(self, inherit: Option<&HoistedScope>) -> HoistedExpr {
		match self {
			Expr::CharLiteral(a, b) => Expr::CharLiteral(a, b),
			Expr::StringLiteral(a, b) => Expr::StringLiteral(a, b),
			Expr::NumberLiteral(a, b) => Expr::NumberLiteral(a, b),
			Expr::Identifier(a, b) => Expr::Identifier(a, b),
			Expr::BinaryOp(a, b, c, d) => {
				Expr::BinaryOp(a, Box::new(b.hoist(inherit)), c, Box::new(d.hoist(inherit)))
			}
			Expr::UnaryOp(a, b, c) => Expr::UnaryOp(a, b, Box::new(c.hoist(inherit))),
			Expr::Lambda(a, b) => Expr::Lambda(a, b.hoist(inherit)),
			Expr::Call(a, b, c, d) => Expr::Call(
				a,
				Box::new(b.hoist(inherit)),
				c.map(|x| x.iter().map(|y| y.clone().hoist(inherit)).collect()),
				d.iter().map(|x| x.clone().hoist(inherit)).collect(),
			),
			Expr::Dot(a, b, c) => {
				Expr::Dot(a, Box::new(b.hoist(inherit)), Box::new(c.hoist(inherit)))
			}
		}
	}
}

impl ParserTypedIdent {
	pub fn hoist(self, inherit: Option<&HoistedScope>) -> HoistedTypedIdent {
		HoistedTypedIdent {
			span: self.span,
			ty: self.ty.hoist(inherit),
			ident: self.ident,
		}
	}
}

impl ParserFuncSignature {
	pub fn hoist(self, inherit: Option<&HoistedScope>) -> HoistedFuncSignature {
		HoistedFuncSignature {
			generics: self.generics,
			arg_tys: self.arg_tys.iter().map(|x| x.clone().hoist(inherit)).collect(),
			return_ty: self.return_ty.hoist(inherit),
		}
	}
}

impl ParserType {
	pub fn hoist(self, inherit: Option<&HoistedScope>) -> HoistedType {
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
			// TODO: hoist func signature
			Type::Function(s, sig) => Type::Function(s, Box::new(sig.hoist(inherit))),
			Type::Inferred(s) => Type::Inferred(s),
		}
	}
}

impl ParserFunc {
	pub fn hoist(self, inherit: Option<&HoistedScope>) -> HoistedFunc {
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

fn redeclaration_error(name: &str, span: &Span, decl_span: Ref<Span>) {
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
	inherit: Option<&HoistedScope>,
) -> (HoistedScope, Vec<Diagnostic<usize>>) {
	// FIXME: make the current hoisted scope into an rc at the beginning and return
	// rcs to hoisted scopes maybe to stop cloning scopes!!
	let hoisted = HoistedScope {
		data: RefCell::new(HoistedScopeData::default()),
		stmts: RefCell::new(vec![]),
		// TODO: get rid of this .clone(); i don't know the rust magic to do it
		// TODO: also prob get rid of this box lol
		inherit: inherit.map(|x| Rc::new(x.clone())),
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
				hoisted.add_var_mutability(&ty_ident.ident_str(), is_mut);
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
				hoisted.add_var_mutability(&ty_ident.ident_str(), is_mut);
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
				// TODO: inherit the hoistation
				let hoisted_scope = hoist(body.clone(), Some(&hoisted)).0;
				let vars = hoisted_scope.data.borrow().vars.clone();
				let funcs = hoisted_scope.data.borrow().funcs.clone();
				let sig = MadeTypeSignature {
					generics: generics.clone(),
					fields: vars,
					funcs,
				};
				hoisted.add_type(&ident.to_string(), sig);
				hoisted.add_type_span(&ident.to_string(), decl_span.clone());
				for_later.push(Stmt::Class(span, privacy, ident, generics, decl_span, body));
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
				let frs = HoistedScope {
					data: RefCell::new(HoistedScopeData::default()),
					stmts: RefCell::new(vec![]),
					// TODO: ouchies this clone hurts
					inherit: Some(Rc::new(hoisted.clone())),
					span: func.decl_span.clone(),
				};
				let mut generics = Vec::new();
				for generic in &func.generics {
					// TODO: deal with discarded generics
					generics.push(generic.clone());
					let name = generic.to_string();
					frs.add_type(
						&name,
						MadeTypeSignature {
							generics: vec![],
							// NOTE: maybe change this
							fields: HashMap::new(),
							funcs: HashMap::new(),
						},
					);
					frs.add_type_span(&name, generic.span());
				}
				for arg in &func.args {
					// TODO: deal with discarded args
					frs.add_var(&arg.ident_str(), arg.ty.clone().hoist(Some(&frs)));
					frs.add_var_span(&arg.ident_str(), arg.span.clone());
				}
				let hoisted_func = func.hoist(Some(&frs));
				hoisted
					.stmts_mut()
					.push(HoistedStmt::Func(span, privacy, ident, hoisted_func));
			}
			Stmt::Class(span, privacy, ident, generics, decl_span, body) => {
				let crs = HoistedScope {
					data: RefCell::new(HoistedScopeData::default()),
					stmts: RefCell::new(vec![]),
					// TODO: ouchies this clone hurts
					inherit: Some(Rc::new(hoisted.clone())),
					span: decl_span.clone(),
				};
				for generic in generics.iter() {
					// TODO: deal with discarded generics
					let name = generic.to_string();
					crs.add_type(
						&name,
						MadeTypeSignature {
							generics: vec![],
							// NOTE: maybe change this
							fields: HashMap::new(),
							funcs: HashMap::new(),
						},
					);
					crs.add_type_span(&name, generic.span());
				}
				let hoisted_scope = hoist(body, Some(&crs)).0;
				hoisted.stmts_mut().push(HoistedStmt::Class(
					span,
					privacy,
					ident,
					generics,
					decl_span,
					hoisted_scope,
				));
			}
			_ => unreachable!(),
		}
	}
	if !DIAGNOSTICS.lock().unwrap().is_empty() {
		let diagnostics = DIAGNOSTICS.lock().unwrap();
		(hoisted, diagnostics.clone())
	} else {
		(hoisted, vec![])
	}
}
