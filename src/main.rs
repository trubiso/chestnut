use chumsky::Stream;
use codespan_reporting::diagnostic::{Diagnostic, Severity};
use codespan_reporting::files::SimpleFiles;
use codespan_reporting::term;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use std::fs;

use crate::parser::CodeStream;
use crate::span::Span;

pub mod codegen;
pub mod common;
pub mod hoister;
pub mod infer;
pub mod lexer;
pub mod parser;
pub mod resolve;
pub mod span;

// TODO: store all lex, ast, parse and resolve in hashmaps that can be accessed
// by the next passes. also do hashmap between source name & file id

fn time<F, T>(should_time: bool, name: &str, x: F) -> T
where
	F: FnOnce() -> T,
{
	if !should_time {
		return x();
	}
	let begin = std::time::Instant::now();
	let ret = x();
	let end = std::time::Instant::now();
	let duration = end - begin;
	println!("{name}: {}µs", duration.as_micros());
	ret
}

fn emit_errors(files: &SimpleFiles<String, String>, diagnostics: Vec<Diagnostic<usize>>) {
	let writer = StandardStream::stderr(ColorChoice::Always);
	let config = term::Config::default();
	let amount = diagnostics.len();
	let warnings = diagnostics
		.iter()
		.filter(|x| x.severity == Severity::Warning)
		.count();
	for diagnostic in diagnostics {
		term::emit(&mut writer.lock(), &config, files, &diagnostic).unwrap();
	}
	println!(
		"{amount} diagnostic{} total ({warnings} warning{}, {} error{})",
		if amount != 1 { "s" } else { "" },
		if warnings != 1 { "s" } else { "" },
		amount - warnings,
		if amount - warnings != 1 { "s" } else { "" },
	);
}

fn execute_cmd_on_all(name: &str, args: Vec<&str>, cpp_sources: Vec<String>) {
	let mut cmd = std::process::Command::new(name);
	for arg in args {
		cmd.arg(arg);
	}
	for file in &cpp_sources {
		cmd.arg(file);
	}
	let out = cmd.output();
	match out {
		Ok(r) => {
			if !r.status.success() {
				println!(
					"{} error!\n{}\n{}",
					name,
					"-".repeat(name.len() + 7),
					String::from_utf8(r.stderr).unwrap()
				);
			}
		}
		Err(x) => panic!("{x}"),
	}
}

fn main() {
	let mut files = SimpleFiles::new();
	let mut args = std::env::args();
	args.next();
	let mut cpp_sources = Vec::new();
	let mut should_format = false;
	let mut all_diagnostics = Vec::new();
	let mut have_errors = false;
	let mut should_time = false;
	for arg in args {
		if arg == "--pretty" {
			should_format = true;
			continue;
		}

		// TODO: this breaks if you don't put it before your files
		if arg == "-t" {
			should_time = true;
			continue;
		}

		let code = fs::read_to_string(arg.clone()).unwrap();
		let file_id = files.add(arg.clone(), code);

		let lexed = match time(should_time, "lexer", || {
			lexer::lex(files.get(file_id).unwrap().source(), file_id)
		}) {
			Ok(x) => x,
			Err((x, diagnostics)) => {
				for diagnostic in diagnostics {
					all_diagnostics.push(diagnostic);
				}
				x
			}
		};

		let code_len = files.get(file_id).unwrap().source().len();

		// TODO: figure out chumsky error recovery strategies
		let lexed_iter: CodeStream =
			Stream::from_iter(Span::new(file_id, code_len..code_len), lexed.into_iter());
		let parsed = match time(should_time, "parser", || parser::parse(lexed_iter)) {
			Ok(x) => x,
			Err((x, diagnostics)) => {
				for diagnostic in diagnostics {
					all_diagnostics.push(diagnostic);
				}
				x
			}
		};

		let (hoisted, hoisted_diagnostics) =
			time(should_time, "hoister", || hoister::hoist(parsed, None));
		for diagnostic in hoisted_diagnostics {
			all_diagnostics.push(diagnostic);
		}

		let infer_diagnostics = time(should_time, "infer", || {
			infer::infer(hoisted.clone())
		});
		for diagnostic in infer_diagnostics {
			all_diagnostics.push(diagnostic);
		}

		// TODO: replace the resolver
		let ((resolved, _), resolved_diagnostics) = time(should_time, "resolver", || {
			resolve::resolve(hoisted, resolve::Context::TopLevel, None)
		});
		for diagnostic in resolved_diagnostics {
			all_diagnostics.push(diagnostic);
		}

		let current_has_errors = all_diagnostics
			.iter()
			.any(|x| x.severity == Severity::Error);
		have_errors = have_errors || current_has_errors;
		if !current_has_errors {
			let code = time(should_time, "codegen", || codegen::codegen(resolved));

			std::fs::write(format!("{arg}.cpp"), code).unwrap();
			cpp_sources.push(format!("{arg}.cpp"));
		}
	}

	if !all_diagnostics.is_empty() {
		if have_errors {
			emit_errors(&files, all_diagnostics);
			println!("errors present, cannot compile :(");
			return;
		} else {
			emit_errors(&files, all_diagnostics);
		}
	}

	if should_format {
		time(should_time, "clang-format", || {
			execute_cmd_on_all("clang-format", vec!["-i"], cpp_sources.clone())
		});
	}
	time(should_time, "g++", || {
		execute_cmd_on_all("g++", vec![], cpp_sources)
	});
}
