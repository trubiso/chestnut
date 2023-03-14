use super::ident::{ident, ident_nodiscard};
use super::ty::ty;
use super::types::{ExprRecursive, ParserTypedIdent};
use chumsky::Parser;

/// Parses `<ty> <ident>` into ParserTypedIdent
pub fn ty_ident(er: Option<ExprRecursive>) -> token_parser!(ParserTypedIdent : '_) {
	ty(er)
		.then(ident())
		.map_with_span(|(ty, ident), span| ParserTypedIdent { span, ty, ident })
}

/// Parses `<ty> <ident_nodiscard>` into ParserTypedIdent
pub fn ty_ident_nodiscard(er: Option<ExprRecursive>) -> token_parser!(ParserTypedIdent : '_) {
	ty(er)
		.then(ident_nodiscard())
		.map_with_span(|(ty, ident), span| ParserTypedIdent { span, ty, ident })
}
