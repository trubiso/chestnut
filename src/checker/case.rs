use super::add_diagnostic;
use crate::{parser::types::Ident, span::Span};
use codespan_reporting::diagnostic::{Diagnostic, Label};
use derive_more::Display;

#[derive(Display, PartialEq)]
pub enum Case {
	#[display(fmt = "pascal case")]
	PascalCase,
	#[display(fmt = "snake case")]
	SnakeCase,
	#[display(fmt = "snake or camel case")]
	SnakeCamel,
	#[display(fmt = "camel case")]
	CamelCase,
	#[display(fmt = "upper snake case")]
	UpperSnakeCase,
}

fn is_uppercase(str: &str) -> bool {
	for char in str.chars() {
		if char.is_alphabetic() && char.is_lowercase() {
			return false;
		}
	}
	true
}

fn has_uppercase(str: &str) -> bool {
	for char in str.chars() {
		if char.is_alphabetic() && char.is_uppercase() {
			return true;
		}
	}
	false
}

fn begins_with_uppercase(str: &str) -> bool {
	str.chars().next().unwrap().is_uppercase()
}

pub fn check_case(span: Span, name: String, wanted: Case) {
	let found = match wanted {
		Case::PascalCase => {
			if name.contains('_') {
				if is_uppercase(&name) {
					Case::UpperSnakeCase
				} else {
					Case::SnakeCase
				}
			} else if !begins_with_uppercase(&name) {
				if has_uppercase(&name) {
					Case::CamelCase
				} else {
					Case::SnakeCamel
				}
			} else {
				Case::PascalCase
			}
		}
		Case::SnakeCase => {
			if has_uppercase(&name) {
				if name.contains('_') && !is_uppercase(&name) {
					Case::SnakeCase
				} else if name.contains('_') {
					Case::UpperSnakeCase
				} else if begins_with_uppercase(&name) {
					Case::PascalCase
				} else {
					Case::CamelCase
				}
			} else {
				Case::SnakeCase
			}
		}
		Case::UpperSnakeCase => {
			if !is_uppercase(&name) {
				if name.contains('_') {
					Case::SnakeCase
				} else {
					Case::PascalCase
				}
			} else {
				Case::UpperSnakeCase
			}
		}
		_ => panic!("why"),
	};
	if found != wanted {
		add_diagnostic(
			Diagnostic::warning()
				.with_message("wrong case system used")
				.with_labels(vec![Label::primary(span.file_id, span.range())
					.with_message(format!("expected {wanted}, found {found}",))]),
		)
	}
}

pub fn check_case_ident(ident: &Ident, wanted: Case) {
	check_case(ident.span(), ident.to_string(), wanted)
}
