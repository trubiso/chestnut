#![feature(trait_alias)]

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

fn emit_errors(files: &SimpleFiles<&str, &String>, diagnostics: Vec<Diagnostic<usize>>) {
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
	let code = fs::read_to_string("code").unwrap();
	let file_id = files.add("code", &code);

	let now = std::time::Instant::now();
	eprint!("Lexing...");

	let lexed = match lexer::lex(&code, file_id) {
		Ok(x) => x,
		Err(x) => return emit_errors(&files, x),
	};

	eprintln!("\rLexed in {}ms", now.elapsed().as_millis());
	let now = std::time::Instant::now();
	eprint!("Parsing...");

	let lexed_iter: CodeStream = Stream::from_iter(
		Span::new(file_id, code.len()..code.len()),
		lexed.into_iter(),
	);
	let parsed = match parser::parse(lexed_iter) {
		Ok(x) => x,
		Err(x) => return emit_errors(&files, x),
	};

	eprintln!("\rParsed in {}ms", now.elapsed().as_millis());
	let now = std::time::Instant::now();
	eprint!("Resolving...");

	let resolved = match resolve::resolve(parsed, resolve::Context::TopLevel, None, None) {
		Ok((x, _)) => x,
		Err(x) => return emit_errors(&files, x),
	};

	eprintln!("\rResolved in {}ms", now.elapsed().as_millis());
	let now = std::time::Instant::now();
	eprint!("Codegenning...");

	let code = codegen::codegen(resolved);

	eprintln!("\rCodegenned in {}ms", now.elapsed().as_millis());

	println!("{code}");
}
