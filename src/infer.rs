use crate::{
	common::{BuiltinType, Expr, NumberLiteralKindKind, Stmt, Type},
	hoister::{HoistedExpr, HoistedScope, HoistedType},
	lexer::NumberLiteralKind,
	span::Span,
};
use codespan_reporting::diagnostic::{Diagnostic, Label};
use lazy_static::lazy_static;
use std::{
	collections::HashMap,
	hash::Hash,
	sync::{Mutex, MutexGuard},
};

// sincere thanks to https://play.rust-lang.org/?version=stable&mode=debug&edition=2018&gist=174ca95a8b938168764846e97d5e9a2c

type InferTypeId = usize;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum InferTypeInfo {
	/// This type is completely unknown.
	Unknown,
	/// This type is known to be the exact same as another type.
	SameAs(InferTypeId),
	/// This type is known to be any signed number type.
	AnySigned,
	/// This type is known to be any unsigned number type.
	AnyUnsigned,
	/// This type is known to be any float number type.
	AnyFloat,
	/// This type is known to be one of the numerous number types.
	KnownNumber(NumberLiteralKind),
	/// This type is known to be `void`.
	KnownVoid,
	/// This type is known to be `bool`.
	KnownBool,
	/// This type is known to be `string`.
	KnownString,
	/// This type is known to be `char`.
	KnownChar,
	/// This type is a reference to another type, analogous to `&x` in code.
	Ref(InferTypeId),
	/// This type is passed as a generic to a function or a class. It does not
	/// unify with anything (in the future it will be able to have trait
	/// constraints which will let it unify with more types). The name is passed
	/// in to avoid unifying unrelated generics.
	Generic(String),
	/// This type is an unknown generic that we're trying to resolve in a
	/// function call, or perhaps the instantiation of a struct.
	UnknownGeneric(String),
	/// This type is an unknown generic that has been resolved to another type.
	/// Practically, it acts like SameAs, except it lets us track what was
	/// resolved.
	ResolvedGeneric(String, InferTypeId),
	/// This type succeeds unification with everything, it is used to avoid
	/// throwing more errors than necessary.
	Bottom,
}

trait ReverseHashMap<K, V> {
	fn reverse(&self) -> HashMap<V, K>;
}

impl<K: Clone, V: Clone + Eq + Hash> ReverseHashMap<K, V> for HashMap<K, V> {
	fn reverse(&self) -> HashMap<V, K> {
		self.iter()
			.map(|(a, b)| (b.clone(), a.clone()))
			.collect::<HashMap<V, K>>()
	}
}

impl InferTypeInfo {
	pub fn display(&self, engine: &InferEngine, idents: &HashMap<String, InferTypeId>) -> String {
		match self {
			Self::Unknown => "?".into(),
			Self::SameAs(x) => {
				let reversed_idents = idents.reverse();
				let name = reversed_idents.get(x).cloned().unwrap_or("anon".into());
				format!("={name} ({})", engine.tys[x].display(engine, idents))
			}
			Self::AnySigned => "int".into(),
			Self::AnyUnsigned => "uint".into(),
			Self::AnyFloat => "float".into(),
			Self::KnownNumber(x) => format!("{}", x.as_ty()),
			Self::Ref(x) => {
				let reversed_idents = idents.reverse();
				let name = reversed_idents.get(x).cloned().unwrap_or("anon".into());
				format!("&{name} ({})", engine.tys[x].display(engine, idents))
			}
			Self::KnownVoid => "void".into(),
			Self::KnownBool => "bool".into(),
			Self::KnownString => "string".into(),
			Self::KnownChar => "char".into(),
			Self::Generic(x) => format!("GENERIC {x}"),
			Self::UnknownGeneric(x) => format!("GENERIC {x}?"),
			Self::ResolvedGeneric(x, y) => {
				let reversed_idents = idents.reverse();
				let name = reversed_idents.get(y).cloned().unwrap_or("anon".into());
				format!(
					"GENERIC {x}! = {name} ({})",
					engine.tys[y].display(engine, idents)
				)
			}
			Self::Bottom => "BOTTOM".into(),
		}
	}

	pub fn display_follow_ref(&self, engine: &InferEngine) -> String {
		match self {
			Self::Unknown => "unknown type".into(),
			Self::SameAs(x) => engine.tys[x].display_follow_ref(engine),
			Self::AnySigned => "int".into(),
			Self::AnyUnsigned => "unsigned int".into(),
			Self::AnyFloat => "float".into(),
			Self::KnownNumber(x) => format!("numeric type {}", x.as_ty()),
			Self::Ref(x) => engine.tys[x].display_follow_ref(engine),
			Self::KnownVoid => "void".into(),
			Self::KnownBool => "bool".into(),
			Self::KnownString => "string".into(),
			Self::KnownChar => "char".into(),
			Self::Generic(x) => format!("generic type {x}"),
			Self::UnknownGeneric(x) => format!("unknown generic type {x}"),
			Self::ResolvedGeneric(x, y) => format!(
				"generic type {x} resolved to {}",
				engine.tys[y].display_follow_ref(engine)
			),
			Self::Bottom => "bottom type".into(),
		}
	}

	pub fn follow_ref(&self, engine: &InferEngine) -> InferTypeInfo {
		match self {
			Self::SameAs(x) => engine.tys[x].follow_ref(engine),
			Self::ResolvedGeneric(_, x) => engine.tys[x].follow_ref(engine),
			other => other.clone(),
		}
	}
}

#[derive(Default, Debug)]
pub struct InferEngine {
	id_counter: usize,
	pub tys: HashMap<usize, InferTypeInfo>,
}

impl InferEngine {
	pub fn add_ty(&mut self, info: InferTypeInfo) -> InferTypeId {
		self.id_counter += 1;
		self.tys.insert(self.id_counter, info);
		self.id_counter
	}

	/// NOTE: a := b doesn't mean b := a, use with care
	pub fn unify(
		&mut self,
		a: InferTypeId,
		b: InferTypeId,
		exact: bool,
	) -> Result<InferTypeInfo, String> {
		use InferTypeInfo::*;
		match (&self.tys[&a], &self.tys[&b]) {
			(SameAs(a), _) => self.unify(*a, b, exact),
			(_, SameAs(b)) => self.unify(a, *b, exact),

			(Bottom, _) => Ok(Bottom),
			(_, Bottom) => Ok(Bottom),

			(Generic(a), Generic(b)) if a == b => Ok(Generic(a.clone())),

			(KnownVoid, KnownVoid) => Ok(KnownVoid),
			(KnownBool, KnownBool) => Ok(KnownBool),
			(KnownString, KnownString) => Ok(KnownString),
			(KnownChar, KnownChar) => Ok(KnownChar),

			(Ref(a), Ref(b)) => self.unify(*a, *b, true),

			(KnownNumber(an), KnownNumber(bn)) => {
				if exact {
					if *an != *bn {
						return Err(format!(
							"numeric type {} is not exactly equal to the expected numeric type {}",
							bn.as_ty(),
							an.as_ty()
						));
					} else {
						return Ok(KnownNumber(*an));
					}
				}
				use NumberLiteralKindKind::*;
				let (a_bytes, a_kind) = an.as_useful_infer_info();
				let (b_bytes, b_kind) = bn.as_useful_infer_info();
				let a_result;
				let _b_result;
				(a_result, _b_result) = match (a_kind, b_kind) {
					(Signed, Unsigned) | (Float, Unsigned) | (Float, Signed) => {
						if a_bytes > b_bytes {
							(*an, *an)
						} else {
							return Err(format!("explicit cast required to cast from numeric type {} to numeric type {}", bn.as_ty(), an.as_ty()));
						}
					}
					(Signed, Signed) | (Unsigned, Unsigned) | (Float, Float) => {
						if a_bytes >= b_bytes {
							(*an, *an)
						} else {
							return Err(format!("explicit cast required to cast from numeric type {} to numeric type {}", bn.as_ty(), an.as_ty()));
						}
					}
					(Signed, _) | (Unsigned, _) => {
						return Err(format!(
						"explicit cast required to cast from numeric type {} to numeric type {}",
						bn.as_ty(),
						an.as_ty()
					))
					}
					(_, None) => (*an, *an),
					(None, _) => (*bn, *bn),
				};
				self.tys.insert(a, KnownNumber(a_result));
				Ok(KnownNumber(a_result))
			}

			(UnknownGeneric(x), _) => {
				println!("resolved generic {x} :D");
				let resolution = ResolvedGeneric(x.clone(), b);
				let r = x.clone();
				self.tys.insert(a, resolution);
				Ok(ResolvedGeneric(r, b))
			}
			(_, UnknownGeneric(x)) => {
				println!("resolved generic {x} :D");
				let resolution = ResolvedGeneric(x.clone(), a);
				let r = x.clone();
				self.tys.insert(b, resolution);
				Ok(ResolvedGeneric(r, a))
			}

			(Unknown, _) => {
				self.tys.insert(a, SameAs(b));
				Ok(SameAs(b))
			}
			(_, Unknown) => {
				self.tys.insert(b, SameAs(a));
				Ok(SameAs(a))
			}

			(a, b) => Err(format!(
				"type conflict between {} and {}",
				a.display_follow_ref(self),
				b.display_follow_ref(self)
			)),
		}
	}
}

lazy_static! {
	static ref ENGINE: Mutex<InferEngine> = Mutex::new(InferEngine::default());
	static ref DIAGNOSTICS: Mutex<Vec<Diagnostic<usize>>> = Mutex::new(vec![]);
}

fn engine<'a>() -> MutexGuard<'a, InferEngine> {
	ENGINE.lock().unwrap()
}

fn add_diagnostic(diagnostic: Diagnostic<usize>) {
	DIAGNOSTICS.lock().unwrap().push(diagnostic);
}

fn ty_errorify(
	span: Span,
	custom: Option<(String, Vec<String>)>,
	x: Result<InferTypeInfo, String>,
) -> Option<InferTypeInfo> {
	// TODO: put a secondary label wherever the type was inferred
	if let Err(ref err) = x {
		let custom = custom.unwrap_or_else(|| ("type conflict".into(), vec![]));
		let title = custom.0;
		let mut extra_error_info = custom.1;
		extra_error_info.push(err.clone());
		add_diagnostic(
			Diagnostic::error()
				.with_message(title)
				.with_labels(vec![Label::primary(span.file_id, span.range())])
				.with_notes(extra_error_info),
		);
	}
	x.ok()
}

impl HoistedType {
	pub fn to_infer_info(&self, named_tys: &HashMap<String, InferTypeId>) -> InferTypeInfo {
		match self {
			Type::BareType(_, x) => engine().tys[&named_tys[&x.ident.to_string()]].clone(),
			Type::Builtin(_, x) => match x {
				BuiltinType::I8 => InferTypeInfo::KnownNumber(NumberLiteralKind::I8),
				BuiltinType::I16 => InferTypeInfo::KnownNumber(NumberLiteralKind::I16),
				BuiltinType::I32 => InferTypeInfo::KnownNumber(NumberLiteralKind::I32),
				BuiltinType::I64 => InferTypeInfo::KnownNumber(NumberLiteralKind::I64),
				BuiltinType::I128 => InferTypeInfo::KnownNumber(NumberLiteralKind::I128),
				BuiltinType::IZ => InferTypeInfo::KnownNumber(NumberLiteralKind::IZ),
				BuiltinType::U8 => InferTypeInfo::KnownNumber(NumberLiteralKind::U8),
				BuiltinType::U16 => InferTypeInfo::KnownNumber(NumberLiteralKind::U16),
				BuiltinType::U32 => InferTypeInfo::KnownNumber(NumberLiteralKind::U32),
				BuiltinType::U64 => InferTypeInfo::KnownNumber(NumberLiteralKind::U64),
				BuiltinType::U128 => InferTypeInfo::KnownNumber(NumberLiteralKind::U128),
				BuiltinType::UZ => InferTypeInfo::KnownNumber(NumberLiteralKind::UZ),
				BuiltinType::F16 => InferTypeInfo::KnownNumber(NumberLiteralKind::F16),
				BuiltinType::F32 => InferTypeInfo::KnownNumber(NumberLiteralKind::F32),
				BuiltinType::F64 => InferTypeInfo::KnownNumber(NumberLiteralKind::F64),
				BuiltinType::F128 => InferTypeInfo::KnownNumber(NumberLiteralKind::F128),
				// TODO: any number, any signed, any unsigned, any float
				BuiltinType::Void => InferTypeInfo::KnownVoid,
				BuiltinType::Bool => InferTypeInfo::KnownBool,
				BuiltinType::String => InferTypeInfo::KnownString,
				BuiltinType::Char => InferTypeInfo::KnownChar,
				BuiltinType::Error => InferTypeInfo::Unknown,
			},
			// TODO: should we consider mutability?
			Type::Ref(_, x, _) => {
				let infer_info = x.to_infer_info(named_tys);
				let id = engine().add_ty(infer_info);
				InferTypeInfo::Ref(id)
			}
			Type::Inferred(..) => InferTypeInfo::Unknown,
			_ => todo!(),
		}
	}
}

impl HoistedExpr {
	pub fn to_infer_info(
		&self,
		idents: &HashMap<String, InferTypeId>,
		named_tys: &HashMap<String, InferTypeId>,
		scope: &HoistedScope,
	) -> InferTypeInfo {
		match self {
			Self::CharLiteral(..) => InferTypeInfo::KnownChar,
			Self::StringLiteral(..) => InferTypeInfo::KnownString,
			Self::NumberLiteral(span, x) => {
				Type::Builtin(span.clone(), x.kind.as_ty()).to_infer_info(&HashMap::new())
			}
			// TODO: allow for hoisting
			Self::Identifier(_, name) => InferTypeInfo::SameAs(
				*idents
					.get(&name.to_string())
					.unwrap_or_else(|| panic!("couldn't get ident with name {name}")),
			),
			Self::BinaryOp(_, lhs, _op, rhs) => {
				// TODO: allow for funky operators which return different tys and are between
				// different tys
				let lhs_infer_info = lhs.to_infer_info(idents, named_tys, scope);
				let lhs_ty = engine().add_ty(lhs_infer_info.clone());
				let rhs_infer_info = rhs.to_infer_info(idents, named_tys, scope);
				let rhs_ty = engine().add_ty(rhs_infer_info);
				let span = self.span();
				if ty_errorify(span, None, engine().unify(lhs_ty, rhs_ty, false)).is_none() {
					InferTypeInfo::Bottom
				} else {
					lhs_infer_info
				}
			}
			Self::UnaryOp(_, _op, value) => {
				// TODO: see above TODO
				value.to_infer_info(idents, named_tys, scope)
			}
			// TODO: i don't feel like dealing with lambdas rn
			Self::Lambda(..) => todo!("lambdas"),
			Self::Call(span, lhs, generics, args) => {
				let Expr::Identifier(_, x) = *lhs.clone() else { todo!("lhs can be something else than an identifier! fix!") };
				let name = &x.to_string();
				let Some(func) = scope.get_func(name) else { todo!("this function does not exist! fix!") };
				let func_generics = func.generics.clone();
				let generics = generics
					.clone()
					.unwrap_or_else(|| vec![Type::Inferred(span.clone()); func.generics.len()]);
				let mut named_tys_with_generics = named_tys.clone();
				for (i, generic) in generics.iter().enumerate() {
					let name = func_generics[i].to_string();
					let mut value_info = generic.to_infer_info(named_tys);
					if value_info == InferTypeInfo::Unknown {
						value_info = InferTypeInfo::UnknownGeneric(name.clone());
					}
					let value_ty = engine().add_ty(value_info);
					named_tys_with_generics.insert(name, value_ty);
				}
				let mut arg_tys = vec![];
				let mut lessons = vec![];
				for (i, arg) in args.iter().enumerate() {
					let arg_info = arg.to_infer_info(idents, &named_tys_with_generics, scope);
					let arg_ty = engine().add_ty(arg_info);
					let func_arg = func.arg_tys[i].clone();
					let func_arg_ty = func_arg.to_infer_info(&named_tys_with_generics);
					let expected_arg_ty = engine().add_ty(func_arg_ty);
					if let Some(x) = ty_errorify(
						arg.span(),
						None,
						engine().unify(arg_ty, expected_arg_ty, false),
					) {
						lessons.push(x);
					}
					arg_tys.push(expected_arg_ty);
				}
				let mut named_tys_with_generics = named_tys.clone();
				for generic in func_generics {
					let name = generic.to_string();
					named_tys_with_generics.insert(name.clone(), {
						let mut le_value = None;
						for lesson in lessons.iter() {
							let InferTypeInfo::ResolvedGeneric(current_name, value) = lesson else { panic!("what have you done") };
							if *current_name == name {
								if let Some(current_value) = le_value {
									let current_ty = engine().tys[current_value].clone();
									let engine = engine();
									if current_ty.follow_ref(&engine) != lesson.follow_ref(&engine) {
										panic!("incompatible generics definition (replace with proper error)");
									}
								}
								le_value = Some(value);
							}
						}
						// TODO: stop ignoring the user's info regarding generics
						*le_value.expect("huhu juju (replace with proper error)")
					});
				}
				func.return_ty
					.clone()
					.to_infer_info(&named_tys_with_generics)
			}
			// TODO: dot access
			Self::Dot(..) => todo!("dot access"),
		}
	}
}

// TODO: return useful info
fn infer_inner(
	scope: HoistedScope,
	inherit_idents: Option<&HashMap<String, InferTypeId>>,
	inherit_named_tys: Option<&HashMap<String, InferTypeId>>,
	expected_return_ty: Option<InferTypeId>,
	nicer_error_span: Option<Span>,
) -> Vec<Diagnostic<usize>> {
	// TODO: good error reporting instead of just unwrapping random stuff
	let stmts = scope.stmts.borrow().clone();
	// TODO: hoist named tys from scope.data.types. this will require Type::Generic
	// to work
	let named_tys = inherit_named_tys.cloned().unwrap_or_default();
	let mut idents = inherit_idents.cloned().unwrap_or_default();
	for (name, ty) in &scope.data.borrow().vars {
		let infer_info = ty.to_infer_info(&named_tys);
		let ty = engine().add_ty(infer_info);
		idents.insert(name.clone(), ty);
	}
	let mut has_returned = false;
	for stmt in stmts {
		match stmt {
			Stmt::Create(span, _, ty_ident, _, expr) => {
				let lhs_ty = idents[&ty_ident.ident_str()];
				let rhs_infer_info = expr.to_infer_info(&idents, &named_tys, &scope);
				let rhs_ty = engine().add_ty(rhs_infer_info);
				ty_errorify(span, None, engine().unify(lhs_ty, rhs_ty, false));
			}
			// hoisting will have taken care already
			Stmt::Declare(..) => {}
			Stmt::Set(span, ident, expr) => {
				// TODO: check for setting non-existing symbols
				let name = ident.to_string();
				let lhs_ty = idents[&name];
				let rhs_infer_info = expr.to_infer_info(&idents, &named_tys, &scope);
				let rhs_ty = engine().add_ty(rhs_infer_info);
				ty_errorify(span, None, engine().unify(lhs_ty, rhs_ty, false));
			}
			Stmt::Func(_, _, _, func) => {
				let mut func_idents = idents.clone();
				let mut func_named_tys = named_tys.clone();
				for generic in func.generics {
					let generic_ty = engine().add_ty(InferTypeInfo::Generic(generic.to_string()));
					func_named_tys.insert(generic.to_string(), generic_ty);
				}
				for arg in func.args {
					let arg_infer_info = arg.ty.to_infer_info(&func_named_tys);
					let arg_ty = engine().add_ty(arg_infer_info);
					func_idents.insert(arg.ident_str(), arg_ty);
				}
				let return_ty = func.return_ty.to_infer_info(&func_named_tys);
				let return_ty_id = engine().add_ty(return_ty);
				infer_inner(
					func.body,
					Some(&func_idents),
					Some(&func_named_tys),
					Some(return_ty_id),
					Some(func.decl_span),
				);
			}
			Stmt::Return(span, expr) => {
				has_returned = true;
				if let Some(x) = &expected_return_ty {
					let got_infer_info = expr.to_infer_info(&idents, &named_tys, &scope);
					let got_ty = engine().add_ty(got_infer_info);
					ty_errorify(span, None, engine().unify(*x, got_ty, false));
				} else {
					todo!("error (we're returning in a non-returning thing)");
				}
			}
			_ => {}
		}
	}
	if !has_returned && let Some(x) = &expected_return_ty {
		let got_ty = engine().add_ty(InferTypeInfo::KnownVoid);
		let span = nicer_error_span.unwrap_or(scope.span);
		ty_errorify(span, Some(("no return in non-void function".into(), vec!["missing return statement in function returning non-void".into()])), engine().unify(*x, got_ty, false));
	}
	let engine = engine();
	for (name, ty) in idents.iter() {
		let ty = engine.tys.get(ty);
		println!("{name}: {}", ty.unwrap().display(&engine, &idents));
	}
	println!("{} types created", engine.id_counter);
	if !DIAGNOSTICS.lock().unwrap().is_empty() {
		let diagnostics = DIAGNOSTICS.lock().unwrap();
		diagnostics.clone()
	} else {
		vec![]
	}
}

pub fn infer(scope: HoistedScope) -> Vec<Diagnostic<usize>> {
	infer_inner(scope, None, None, None, None)
}
