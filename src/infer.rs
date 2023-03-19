use crate::{
	common::{BuiltinType, NumberLiteralKindKind, Stmt, Type},
	hoister::{HoistedExpr, HoistedScope, HoistedType},
	lexer::NumberLiteralKind,
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

#[derive(Debug)]
pub enum InferTypeInfo {
	Unknown,
	SameAs(InferTypeId),
	AnySigned,
	AnyUnsigned,
	AnyFloat,
	KnownNumber(NumberLiteralKind),
	KnownVoid,
	KnownBool,
	KnownString,
	KnownChar,
	Ref(InferTypeId),
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
	pub fn display(&self, engine: &InferEngine, idents: &HashMap<String, usize>) -> String {
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
	pub fn unify(&mut self, a: InferTypeId, b: InferTypeId) -> Result<(), String> {
		use InferTypeInfo::*;
		match (&self.tys[&a], &self.tys[&b]) {
			(SameAs(a), _) => self.unify(*a, b),
			(_, SameAs(b)) => self.unify(a, *b),

			(Ref(a), Ref(b)) => self.unify(*a, *b),

			(KnownNumber(an), KnownNumber(bn)) => {
				use NumberLiteralKindKind::*;
				let (a_bytes, a_kind) = an.as_useful_infer_info();
				let (b_bytes, b_kind) = bn.as_useful_infer_info();
				let a_result;
				let _b_result;
				(a_result, _b_result) = match (a_kind, b_kind) {
					(Signed, Unsigned) | (Float, Unsigned) | (Float, Signed) => if a_bytes > b_bytes {
						(*an, *an)
					} else {
						return Err(format!("explicit cast required to cast from number type {bn:?} to number type {an:?}"))
					}
					(Signed, Signed) | (Unsigned, Unsigned) | (Float, Float) => if a_bytes >= b_bytes {
						(*an, *an)
					} else {
						return Err(format!("explicit cast required to cast from number type {bn:?} to number type {an:?}"))
					}
					(Signed, _) | (Unsigned, _) => return Err(format!("explicit cast required to cast from number type {bn:?} to number type {an:?}")),
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

			(a, b) => Err(format!("conflict between {a:?} and {b:?}")),
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

impl HoistedType {
	pub fn to_infer_info(&self) -> InferTypeInfo {
		match self {
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
			Type::Ref(_, x, _) => InferTypeInfo::Ref(engine().add_ty(x.to_infer_info())),
			Type::Inferred(..) => InferTypeInfo::Unknown,
			_ => todo!(),
		}
	}
}

impl HoistedExpr {
	pub fn to_infer_info(&self, idents: &HashMap<String, usize>) -> InferTypeInfo {
		match self {
			Self::CharLiteral(span, _) => {
				Type::Builtin(span.clone(), BuiltinType::Char).to_infer_info()
			}
			Self::StringLiteral(span, _) => {
				Type::Builtin(span.clone(), BuiltinType::String).to_infer_info()
			}
			Self::NumberLiteral(span, x) => {
				Type::Builtin(span.clone(), x.kind.as_ty()).to_infer_info()
			}
			// TODO: allow for hoisting
			Self::Identifier(_, name) => InferTypeInfo::SameAs(
				*idents
					.get(&name.to_string())
					.expect(&format!("couldn't get ident with name {name}")),
			),
			Self::BinaryOp(_, lhs, _op, _rhs) => {
				// TODO: allow for funky operators which return different tys and are between
				// different tys
				lhs.to_infer_info(idents)
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
pub fn infer(
	scope: HoistedScope,
	inherit_idents: Option<&HashMap<String, usize>>,
) -> Vec<Diagnostic<usize>> {
	// TODO: good error reporting instead of just unwrapping random stuff
	let stmts = scope.stmts.borrow().clone();
	let mut idents = inherit_idents.cloned().unwrap_or_default();
	for stmt in stmts {
		match stmt {
			Stmt::Create(span, _, ty_ident, _, expr) => {
				let name = ty_ident.ident_str();
				let lhs_infer_info = ty_ident.ty.to_infer_info();
				let lhs_ty = engine().add_ty(lhs_infer_info);
				idents.insert(name, lhs_ty);
				let rhs_infer_info = expr.to_infer_info(&idents);
				let rhs_ty = engine().add_ty(rhs_infer_info);
				match engine().unify(lhs_ty, rhs_ty) {
					Err(err) => add_diagnostic(
						Diagnostic::error()
							.with_message(err)
							.with_labels(vec![Label::primary(span.file_id, span.range())
								.with_message("type error occurred here")]),
					),
					Ok(_) => {}
				}
			}
			Stmt::Declare(_, _, ty_ident, _) => {
				let name = ty_ident.ident_str();
				let lhs_infer_info = ty_ident.ty.to_infer_info();
				let lhs_ty = engine().add_ty(lhs_infer_info);
				idents.insert(name, lhs_ty);
			}
			Stmt::Set(span, ident, expr) => {
				let name = ident.to_string();
				let lhs_ty = idents[&name];
				let rhs_infer_info = expr.to_infer_info(&idents);
				let rhs_ty = engine().add_ty(rhs_infer_info);
				match engine().unify(lhs_ty, rhs_ty) {
					Err(err) => add_diagnostic(
						Diagnostic::error()
							.with_message(err)
							.with_labels(vec![Label::primary(span.file_id, span.range())
								.with_message("type error occurred here")]),
					),
					Ok(_) => {}
				}
			}
			Stmt::Func(_, _, _, func) => {
				infer(func.body, Some(&idents));
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
