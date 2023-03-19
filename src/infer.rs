use crate::{
	common::{BuiltinType, NumberLiteralKindKind, Stmt, Type},
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

#[derive(Debug, Clone)]
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
	/// constraints which will let it unify with more types)
	Generic,
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
			Self::Generic => "GENERIC".into(),
			Self::Bottom => "BOTTOM".into(),
		}
	}

	pub fn display_follow_ref(
		&self,
		engine: &InferEngine,
		idents: &HashMap<String, InferTypeId>,
	) -> String {
		match self {
			Self::Unknown => "unknown type".into(),
			Self::SameAs(x) => engine.tys[x].display_follow_ref(engine, idents),
			Self::AnySigned => "int".into(),
			Self::AnyUnsigned => "unsigned int".into(),
			Self::AnyFloat => "float".into(),
			Self::KnownNumber(x) => format!("numeric type {}", x.as_ty()),
			Self::Ref(x) => engine.tys[x].display_follow_ref(engine, idents),
			Self::KnownVoid => "void".into(),
			Self::KnownBool => "bool".into(),
			Self::KnownString => "string".into(),
			Self::KnownChar => "char".into(),
			Self::Generic => "generic type".into(),
			Self::Bottom => "bottom type".into(),
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
		idents: &HashMap<String, usize>,
	) -> Result<(), String> {
		use InferTypeInfo::*;
		match (&self.tys[&a], &self.tys[&b]) {
			(SameAs(a), _) => self.unify(*a, b, exact, idents),
			(_, SameAs(b)) => self.unify(a, *b, exact, idents),

			(Bottom, _) => Ok(()),
			(_, Bottom) => Ok(()),

			(Generic, Generic) => Ok(()),

			(KnownVoid, KnownVoid) => Ok(()),
			(KnownBool, KnownBool) => Ok(()),
			(KnownString, KnownString) => Ok(()),
			(KnownChar, KnownChar) => Ok(()),

			(Ref(a), Ref(b)) => self.unify(*a, *b, true, idents),

			(KnownNumber(an), KnownNumber(bn)) => {
				if exact {
					if *an != *bn {
						return Err(format!(
							"numeric type {} is not exactly equal to the expected numeric type {}",
							bn.as_ty(),
							an.as_ty()
						));
					} else {
						return Ok(());
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
				Ok(())
			}

			(Unknown, _) => {
				self.tys.insert(a, SameAs(b));
				Ok(())
			}
			(_, Unknown) => {
				self.tys.insert(b, SameAs(a));
				Ok(())
			}

			(a, b) => Err(format!(
				"type conflict between {} and {}",
				a.display_follow_ref(self, idents),
				b.display_follow_ref(self, idents)
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

fn ty_errorify(span: Span, x: Result<(), String>) -> bool {
	// TODO: put a secondary label wherever the type was inferred
	match x {
		Err(err) => {
			add_diagnostic(
				Diagnostic::error()
					.with_message("type conflict")
					.with_labels(vec![Label::primary(span.file_id, span.range())])
					.with_notes(vec![err]),
			);
			true
		}
		Ok(_) => false,
	}
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
			Type::Ref(_, x, _) => InferTypeInfo::Ref(engine().add_ty(x.to_infer_info(named_tys))),
			Type::Inferred(..) => InferTypeInfo::Unknown,
			_ => todo!(),
		}
	}
}

impl HoistedExpr {
	pub fn to_infer_info(&self, idents: &HashMap<String, InferTypeId>) -> InferTypeInfo {
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
					.expect(&format!("couldn't get ident with name {name}")),
			),
			Self::BinaryOp(_, lhs, _op, rhs) => {
				// TODO: allow for funky operators which return different tys and are between
				// different tys
				let lhs_infer_info = lhs.to_infer_info(idents);
				let lhs_ty = engine().add_ty(lhs_infer_info.clone());
				let rhs_infer_info = rhs.to_infer_info(idents);
				let rhs_ty = engine().add_ty(rhs_infer_info);
				let span = self.span();
				if ty_errorify(span, engine().unify(lhs_ty, rhs_ty, false, idents)) {
					InferTypeInfo::Bottom
				} else {
					lhs_infer_info
				}
			}
			Self::UnaryOp(_, _op, value) => {
				// TODO: see above TODO
				value.to_infer_info(idents)
			}
			// TODO: i don't feel like dealing with lambdas rn
			Self::Lambda(..) => todo!("lambdas"),
			// TODO: function calls
			Self::Call(..) => todo!("function calls"),
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
	for stmt in stmts {
		match stmt {
			Stmt::Create(span, _, ty_ident, _, expr) => {
				let lhs_ty = idents[&ty_ident.ident_str()];
				let rhs_infer_info = expr.to_infer_info(&idents);
				let rhs_ty = engine().add_ty(rhs_infer_info);
				ty_errorify(span, engine().unify(lhs_ty, rhs_ty, false, &idents));
			}
			// hoisting will have taken care already
			Stmt::Declare(..) => {}
			Stmt::Set(span, ident, expr) => {
				// TODO: check for setting non-existing symbols
				let name = ident.to_string();
				let lhs_ty = idents[&name];
				let rhs_infer_info = expr.to_infer_info(&idents);
				let rhs_ty = engine().add_ty(rhs_infer_info);
				ty_errorify(span, engine().unify(lhs_ty, rhs_ty, false, &idents));
			}
			Stmt::Func(_, _, _, func) => {
				let mut func_idents = idents.clone();
				let mut func_named_tys = named_tys.clone();
				for generic in func.generics {
					let generic_ty = engine().add_ty(InferTypeInfo::Generic);
					func_named_tys.insert(generic.to_string(), generic_ty);
				}
				for arg in func.args {
					let arg_infer_info = arg.ty.to_infer_info(&func_named_tys);
					let arg_ty = engine().add_ty(arg_infer_info);
					func_idents.insert(arg.ident_str(), arg_ty);
				}
				infer_inner(func.body, Some(&func_idents), Some(&func_named_tys));
			}
			_ => {}
		}
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
	infer_inner(scope, None, None)
}
