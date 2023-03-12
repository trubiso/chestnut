use super::types::{BuiltinType, ExprRecursive, Type};
use crate::lexer::Token;
use crate::parser::expr;
use crate::parser::types::{BareType, Expr};
use chumsky::prelude::*;

enum PostfixOp {
	Array(Option<Expr>),
	Optional,
	Ref,
	RefRef,
	MutRef,
	MutRefRef,
}

/// Parses `<ident>[array][optional][ref][mut]` into Type
pub fn ty(er: Option<ExprRecursive>) -> token_parser!(Type : '_) {
	recursive(|ty| {
		filter(|token| matches!(token, Token::Identifier(_)) || *token == keyword!(DontCare))
			.then(angled!(ty,).or_not())
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
								generics: generics.unwrap_or(vec![]).iter().cloned().collect(),
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
				PostfixOp::Array(x) => Type::Array(span, Box::new(ty), x.map(Box::new)),
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
	})
}
