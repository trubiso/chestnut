use super::types::{Ident};
use crate::lexer::Token;
use chumsky::prelude::*;

/// Parses an ident token into Ident
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

// TODO: qualified ident (`ident_nodiscard::...`). foldl the ::s into Vec<String>
