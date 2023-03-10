use crate::span::Span;

use super::ident::ident;
use super::ty_ident::ty_ident;
use super::types::{Expr, Func, FuncAttribs, Scope, Stmt, Type};
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
				literal_parser!(Identifier),
			))
		};
		// TODO: dot access parser
		let fc_parser = || {
			span!(atom())
				.then(span!(parened!(e.clone(),)).repeated())
				.foldl(|(lhs, ls), (args, rs)| {
					let span: Span = ls + rs;
					(Expr::Call(span.clone(), Box::new(lhs), args), span)
				})
				.map(|(x, _)| x)
		};
		// TODO: qbparser (?!)
		// TODO: ref_parser (&*) 
		let neg_parser = unop_parser!(Neg => fc_parser);
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
			.map_with_span(|(args, body), span| {
				Expr::Lambda(
					span.clone(),
					Func {
						span: span.clone(),
						return_ty: Type::Inferred(span), // NOTE: there's no span for this type
						body,
						generics: Vec::new(), // NOTE: should lambda generics?
						args,
						attribs: FuncAttribs::default(),
					},
				)
			})
		};
		choice((lambda(), pn_parser())).boxed()
	})
}
