use super::ident::{ident, potentially_qualified_ident};
use super::ty::ty;
use super::ty_ident::ty_ident;
use super::types::{Expr, Func, FuncAttribs, Scope, Stmt, Type};
use crate::span::Span;
use chumsky::error::Simple;
use chumsky::prelude::*;

macro_rules! binop_parser {
	($($op:ident)* => $next:ident) => {
		|| $next()
			.then(
				span!(choice(($(jop!($op),)*)))
				.then($next()).repeated())
				// FIXME: get the proper span of lhs + op + rhs
			.foldl(|lhs, ((op, span), rhs)| Expr::BinaryOp(span, Box::new(lhs), force_token!(op => Operator), Box::new(rhs)))
	};
}

macro_rules! unop_parser {
	($($op:ident)* => $next:ident) => {
		|| span!(choice(($(jop!($op),)*))).repeated()
			.then($next())
			// NOTE: i don't know if this span is correct?
			// TODO: perhaps get rhs span
			.foldr(|(op, s), rhs| Expr::UnaryOp(s, force_token!(op => Operator), Box::new(rhs)))
	};
}

macro_rules! literal_parser {
	($kind:ident) => {
		filter(|x| matches!(x, $crate::lexer::Token::$kind(_))).map_with_span(|x, span: Span| {
			Expr::$kind(span.clone(), force_token!(x => $kind, span))
		})
	};
}

/// Parses an expression into Expr
pub fn expr() -> token_parser!(Expr) {
	// () then - then ! then == != <= >= < > then && then || then *÷ then +-
	recursive(|e| {
		let atom = || {
			choice((
				parened!(e.clone()),
				literal_parser!(StringLiteral),
				literal_parser!(NumberLiteral),
				literal_parser!(CharLiteral),
				potentially_qualified_ident().map(|x| Expr::Identifier(x.span(), x)),
			))
		};
		let fc_parser = || {
			span!(atom())
				.then(
					span!(angled!(ty(Some(e.clone())),)
						.or_not()
						.then(parened!(e.clone(),)))
					.repeated(),
				)
				.foldl(|(lhs, ls), ((generics, args), rs)| {
					let span: Span = ls + rs;
					(
						Expr::Call(span.clone(), Box::new(lhs), generics, args),
						span,
					)
				})
				.map(|(x, _)| x)
		};
		let dot_parser = || {
			span!(fc_parser())
				.then(jpunct!(Dot).ignore_then(span!(fc_parser())).repeated())
				.validate(|stuff: ((Expr, Span), Vec<(Expr, Span)>), _, emit| {
					let mut lhs = stuff.0 .0;
					let mut lhs_span = stuff.0 .1;
					for (rhs, rhs_span) in stuff.1 {
						match rhs {
							Expr::Identifier(..) | Expr::Call(..) => {}
							ref other => {
								emit(Simple::custom(other.span(), r#"invalid property "name""#))
							}
						}
						lhs_span = lhs_span + rhs_span;
						lhs = Expr::Dot(lhs_span.clone(), Box::new(lhs), Box::new(rhs));
					}
					lhs
				})
		};
		// TODO: qbparser (?!)
		// TODO: ref_parser (&*)
		let neg_parser = unop_parser!(Neg => dot_parser);
		let not_parser = unop_parser!(Bang => neg_parser);
		let eq_parser = binop_parser!(Eq Ne Lt Gt Le Ge => not_parser);
		let and_parser = binop_parser!(And => eq_parser);
		let or_parser = binop_parser!(Or => and_parser);
		let sd_parser = binop_parser!(Star Div => or_parser);
		let pn_parser = binop_parser!(Plus Neg => sd_parser);
		let lambda = || {
			parened!(choice((
				ty_ident(Some(e.clone())),
				ident().map(|x| x.infer_type())
			)),)
			.map_with_span(|pre, span| (pre, span))
			.then(choice((
				// bare_scope(),
				jkeyword!(FatArrow)
					.ignore_then(pn_parser())
					.map_with_span(|expr, span| Scope {
						span: span.clone(),
						stmts: vec![Stmt::Return(span, expr)],
					}),
				jkeyword!(FatArrow)
					.ignore_then(pn_parser())
					.map_with_span(|expr, span| Scope {
						span: span.clone(),
						stmts: vec![Stmt::Return(span, expr)],
					}),
			)))
			.map_with_span(|((args, decl_span), body), span| {
				Expr::Lambda(
					span.clone(),
					Func {
						span: span.clone(),
						return_ty: Type::Inferred(span), // NOTE: there's no span for this type
						body,
						generics: Vec::new(), // NOTE: should lambda generics?
						args,
						attribs: FuncAttribs::default(),
						decl_span,
					},
				)
			})
		};
		choice((lambda(), pn_parser())).boxed()
	})
}
