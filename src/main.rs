use chumsky::Stream;
use codespan_reporting::diagnostic::Diagnostic;
use codespan_reporting::files::SimpleFiles;
use codespan_reporting::term;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use std::fs;

use crate::parser::CodeStream;
use crate::span::Span;

pub mod codegen;
pub mod lexer;
pub mod parser;
pub mod resolve;
pub mod span;

// TODO: store all lex, ast, parse and resolve in hashmaps that can be accessed
// by the next passes. also do hashmap between source name & file id

fn emit_errors(files: &SimpleFiles<String, String>, diagnostics: Vec<Diagnostic<usize>>) {
	let writer = StandardStream::stderr(ColorChoice::Always);
	let config = term::Config::default();
	let amount = diagnostics.len();
	for diagnostic in diagnostics {
		term::emit(&mut writer.lock(), &config, files, &diagnostic).unwrap();
	}
	println!(
		"{amount} diagnostic{} total",
		if amount > 1 { "s" } else { "" }
	);
}

fn main() {
	let mut files = SimpleFiles::new();
	let mut args = std::env::args();
	args.next();
	let mut cpp_sources = Vec::new();
	while let Some(arg) = args.next() {
		let code = fs::read_to_string(arg.clone()).unwrap();
		let file_id = files.add(arg.clone(), code);

		let lexed = match lexer::lex(files.get(file_id).unwrap().source(), file_id) {
			Ok(x) => x,
			Err(x) => return emit_errors(&files, x),
		};

		let code_len = files.get(file_id).unwrap().source().len();

		let lexed_iter: CodeStream =
			Stream::from_iter(Span::new(file_id, code_len..code_len), lexed.into_iter());
		let parsed = match parser::parse(lexed_iter) {
			Ok(x) => x,
			Err(x) => return emit_errors(&files, x),
		};

		let resolved = match resolve::resolve(parsed, resolve::Context::TopLevel, None, None) {
			Ok((x, _)) => x,
			Err(x) => return emit_errors(&files, x),
		};

		let code = codegen::codegen(resolved);

		std::fs::write(format!("{arg}.cpp"), code).unwrap();
		cpp_sources.push(format!("{arg}.cpp"));
	}

	let mut clang_format = std::process::Command::new("clang-format");
	clang_format.arg("-i");
	for file in &cpp_sources {
		clang_format.arg(file);
	}
	clang_format.output().expect("failed to clang-format");

	let mut gcc = std::process::Command::new("g++");
	for file in &cpp_sources {
		gcc.arg(file);
	}
	gcc.output().expect("failed to g++");
}
