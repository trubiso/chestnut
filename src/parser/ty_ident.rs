use crate::common::TypedIdent;

use super::ident::{ident, ident_nodiscard};
use super::ty::ty;
use super::types::ExprRecursive;
use chumsky::Parser;

/// Parses `<ty> <ident>` into TypedIdent
pub fn ty_ident(er: Option<ExprRecursive>) -> token_parser!(TypedIdent : '_) {
	ty(er)
		.then(ident())
		.map_with_span(|(ty, ident), span| TypedIdent { span, ty, ident })
}

/// Parses `<ty> <ident_nodiscard>` into TypedIdent
pub fn ty_ident_nodiscard(er: Option<ExprRecursive>) -> token_parser!(TypedIdent : '_) {
	ty(er)
		.then(ident_nodiscard())
		.map_with_span(|(ty, ident), span| TypedIdent { span, ty, ident })
}
