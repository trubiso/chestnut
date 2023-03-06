#![feature(trait_alias)]

use chumsky::Stream;
use codespan_reporting::diagnostic::Diagnostic;
use codespan_reporting::files::SimpleFiles;
use codespan_reporting::term;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use std::fs;

use crate::parser::CodeStream;
use crate::span::Span;

pub mod lexer;
pub mod parser;
pub mod resolve;
pub mod span;

fn emit_errors(files: &SimpleFiles<&str, &String>, diagnostics: Vec<Diagnostic<usize>>) {
	let writer = StandardStream::stderr(ColorChoice::Always);
	let config = term::Config::default();
	for diagnostic in diagnostics {
		term::emit(&mut writer.lock(), &config, files, &diagnostic).unwrap();
	}
}

fn main() {
	let mut files = SimpleFiles::new();
	let code = fs::read_to_string("code").unwrap();
	let file_id = files.add("code", &code);

	let lexed = match lexer::lex(&code, file_id) {
		Ok(x) => x,
		Err(x) => return emit_errors(&files, x),
	};

	let lexed_iter: CodeStream = Stream::from_iter(
		Span::new(file_id, code.len()..code.len()),
		lexed.into_iter(),
	);
	let parsed = match parser::parse(lexed_iter) {
		Ok(x) => x,
		Err(x) => return emit_errors(&files, x),
	};

	println!("{parsed}");

	let _resolved = match resolve::resolve(parsed, resolve::Context::TopLevel, None) {
		Ok(x) => x,
		Err(x) => return emit_errors(&files, x),
	};

	// println!("{parsed}");
}
