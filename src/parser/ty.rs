use super::types::{ExprRecursive, ParserExpr};
use crate::common::{BareType, BuiltinType, FuncSignature, Type};
use crate::lexer::Token;
use crate::parser::expr;
use chumsky::prelude::*;

enum PostfixOp {
	Array(Option<ParserExpr>),
	Optional,
	Ref,
	RefRef,
	MutRef,
	MutRefRef,
}

/// Parses `<ident>[array][optional][ref][mut][(<ty>, ...)]` into Type
pub fn ty(er: Option<ExprRecursive>) -> token_parser!(Type : '_) {
	recursive(|ty| {
		filter(|token| matches!(token, Token::Identifier(_)) || *token == keyword!(DontCare))
			.then(angled!(ty.clone(),).or_not())
			.map_with_span(|(ident, generics), span| {
				if ident == keyword!(DontCare) {
					Type::Inferred(span)
				} else {
					// TODO: incorrect span
					let ident = force_token!(ident => Identifier, span.clone());
					if let Some(x) = BuiltinType::from_name(&ident.to_string()) {
						Type::Builtin(span, x)
					} else {
						Type::BareType(
							span,
							BareType {
								ident,
								generics: generics.unwrap_or(vec![]).to_vec(),
							},
						)
					}
				}
			})
			.then(
				choice((
					bracketed!(er
						.map(|x| x.boxed())
						.unwrap_or_else(|| expr().boxed())
						.or_not())
					.map(PostfixOp::Array),
					jop!(Question).map(|_| PostfixOp::Optional),
					jkeyword!(Mut).then(jop!(Amp)).map(|_| PostfixOp::MutRef),
					jkeyword!(Mut).then(jop!(And)).map(|_| PostfixOp::MutRefRef),
					jop!(Amp).map(|_| PostfixOp::Ref),
					jop!(And).map(|_| PostfixOp::RefRef),
				))
				.map_with_span(|x, s| (x, s))
				.repeated(),
			)
			.foldl(|ty, (new_info, span)| match new_info {
				// TODO: error handling, unwrap is bad in this case
				PostfixOp::Array(x) => {
					Type::Array(span, Box::new(ty), x.map(|x| x.unscoped_box().unwrap()))
				}
				PostfixOp::Optional => Type::Optional(span, Box::new(ty)),
				PostfixOp::MutRef => Type::Ref(span, Box::new(ty), true),
				PostfixOp::MutRefRef => Type::Ref(
					span.clone(),
					Box::new(Type::Ref(span, Box::new(ty), true)),
					false,
				),
				PostfixOp::Ref => Type::Ref(span, Box::new(ty), false),
				PostfixOp::RefRef => Type::Ref(
					span.clone(),
					Box::new(Type::Ref(span, Box::new(ty), false)),
					false,
				),
			})
			.then(parened!(ty,).or_not())
			.map_with_span(|(l, args), span| {
				// TODO: support functions that return functions (foldl)
				// TODO: generics (we might have to change the syntax to accomodate for them)
				match args {
					Some(args) => Type::Function(
						span,
						Box::new(FuncSignature {
							generics: vec![], // see above
							arg_tys: args,
							return_ty: l,
						}),
					),
					None => l,
				}
			})
	})
}
