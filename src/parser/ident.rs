use super::types::Ident;
use crate::lexer::Token;
use chumsky::prelude::*;

/// Parses an ident token into Ident::Named
pub fn ident() -> token_parser!(Ident) {
	filter(|token| matches!(token, Token::Identifier(_)) || *token == keyword!(DontCare))
		.map_with_span(|token, span| {
			if token == keyword!(DontCare) {
				Ident::Discarded(span)
			} else {
				force_token!(token => Identifier, span)
			}
		})
}

/// Parses a non-inferred ident token into Ident
pub fn ident_nodiscard() -> token_parser!(Ident) {
	filter(|token| matches!(token, Token::Identifier(_)))
		.map_with_span(|token, span| force_token!(token => Identifier, span))
}

/// Parses `<ident_nodiscard>::...` into Ident::Qualified
pub fn qualified_ident() -> token_parser!(Ident) {
	// TODO: maybe have a corresponding Vec<Span>
	ident_nodiscard()
		.separated_by(jpunct!(ColonColon))
		.at_least(2)
		.map_with_span(|names, span| Ident::Qualified(span, names))
}

pub fn potentially_qualified_ident() -> token_parser!(Ident) {
	qualified_ident().or(ident_nodiscard())
}
