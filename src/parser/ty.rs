use super::types::{BuiltinType, ExprRecursive, TokenParser, Type};
use crate::lexer::{Keyword, Operator, Token};
use crate::parser::expr;
use crate::parser::types::{BareType, Expr};
use chumsky::prelude::*;

enum PostfixOp {
	Expr(Option<Expr>),
	Operator(Operator),
	Keyword(Keyword),
}

/// Parses `<ident>[array][optional][ref][mut]` into Type
pub fn ty(er: Option<ExprRecursive>) -> impl TokenParser<Type> + '_ {
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
								generics: generics
									.unwrap_or(vec![])
									.iter()
									.cloned()
									.collect(),
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
					.map(PostfixOp::Expr),
					jop!(Question).map(|x| PostfixOp::Operator(force_token!(x => Operator))),
					jop!(Amp).map(|x| PostfixOp::Operator(force_token!(x => Operator))),
					jop!(And).map(|x| PostfixOp::Operator(force_token!(x => Operator))),
					jkeyword!(Mut).map(|x| PostfixOp::Keyword(force_token!(x => Keyword))),
				))
				.map_with_span(|x, s| (x, s))
				.repeated(),
			)
			.foldl(|ty, (new_info, span)| match new_info {
				PostfixOp::Expr(x) => Type::Array(span, Box::new(ty), x.map(Box::new)),
				PostfixOp::Operator(x) => match x {
					Operator::Question => Type::Optional(span, Box::new(ty)),
					Operator::Amp => Type::Ref(span, Box::new(ty)),
					Operator::And => {
						Type::Ref(span.clone(), Box::new(Type::Ref(span, Box::new(ty))))
					}
					// TODO: merge span with ty.span, all of these spans are wrong LOL
					_ => unreachable!(),
				},
				PostfixOp::Keyword(x) => match x {
					Keyword::Mut => Type::Mut(span, Box::new(ty)),
					_ => unreachable!(),
				},
			})
	})
}
