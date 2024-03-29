use crate::common::Privacy;
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
				match x.as_keyword().unwrap() {
					$(Keyword::$kw => Privacy::$kw(span),)*
					other => unreachable!("{other}")
				}
			} else {
				Privacy::Default
			}
		})
	};
}

/// Parses `<private|protected|public|export>` into Privacy
pub fn privacy_attribs() -> token_parser!(Privacy) {
	privacy_qualifiers!(Private Protected Public Export)
}
