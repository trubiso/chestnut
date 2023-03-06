use super::types::{Privacy, TokenParser};
use crate::lexer::Keyword;
use crate::span::Span;
use chumsky::prelude::*;

macro_rules! privacy_qualifiers {
	($($kw:ident)*) => {
		choice(($(jkeyword!($kw),)*))
		.repeated()
		.validate(|attribs, span: Span, emit| {
			if attribs.len() > 1 {
				emit(chumsky::error::Simple::custom(span.clone(), "too many privacy qualifiers"));
			}

			if let Some(x) = attribs.get(0) {
				match force_token!(x => Keyword) {
					$(Keyword::$kw => Privacy::$kw(span),)*
					_ => unreachable!()
				}
			} else {
				Privacy::Default
			}
		})
	};
}

pub fn privacy_attribs() -> impl TokenParser<Privacy> {
	privacy_qualifiers!(Private Protected Public Export)
}
