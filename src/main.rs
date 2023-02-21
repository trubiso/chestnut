#![feature(trait_alias)]

use chumsky::Stream;
use codespan_reporting::diagnostic::Diagnostic;
use codespan_reporting::files::SimpleFiles;
use codespan_reporting::term;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use std::fs;

pub mod lexer;
pub mod parser;

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

	let lexed_iter = Stream::from_iter(code.len()..code.len(), lexed.into_iter());
	let _parsed = match parser::parse(lexed_iter, file_id) {
		Ok(x) => x,
		Err(x) => return emit_errors(&files, x),
	};

	println!("{_parsed}");
}
