use crate::span::{Span, Spanned};
use codespan_reporting::diagnostic::{Diagnostic, Label};
use derive_more::Display;
use logos::{Lexer, Logos};
use std::fmt::Display;

fn lex_to_str<'source>(lexer: &Lexer<'source, Token>) -> String {
	lexer.slice().trim().to_string()
}

#[allow(unused_macros)]
macro_rules! def_token {
	($($vid:ident { $($match:expr => $to:ident,)* })*) => {
		$(
			#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
			pub enum $vid { $($to,)* }
			impl Display for $vid {
				fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
					f.write_str(match *self { $(Self::$to => $match,)* })
				}
			}
		)*
		#[derive(Logos, Debug, Display, PartialEq, Eq, Clone, Hash)]
		pub enum Token {
			#[error]
			#[regex(r"\s+", logos::skip)]
			Error,
			#[regex(r"([0-9][0-9_]*|(?:[0-9][0-9_]*)?\.[0-9][0-9_]*|0b[01][01_]*|0o[0-7][0-7_]*|0x[0-9a-fA-F][0-9a-fA-F_]*)(i(?:z|8|16|32|64|128)|u(?:z|8|16|32|64|128)?|f(?:16|32|64|128)?)?", lex_to_str)]
			NumberLiteral(String),
			#[regex(r"'.'", lex_to_str)]
			CharLiteral(String),
			#[regex(r#""(?:[^"]|\\")*""#, lex_to_str)]
			StringLiteral(String),
			$(
				$(#[token($match, |_| $vid::$to)])*
				$vid($vid),
			)*
			#[regex(r"[a-zA-Z_][a-zA-Z0-9_]*", lex_to_str)]
			Identifier(String),
		}
	};
}

def_token!(
	Keyword {
		"public" => Public,
		"func" => Function,
		"pure" => Pure,
		"return" => Return,
		"->" => Arrow,
		"=>" => FatArrow,
		"let" => Let,
		"const" => Const,
		"for" => For,
		"in" => In,
		"while" => While,
		"loop" => Loop,
		"if" => If,
		"~" => DontCare,
	}

	Operator {
		"?" => Question,
		"!" => Bang,
		"&" => Amp,
		"&&" => And,
		"||" => Or,
		// unary/binary
		"-" => Neg,
		"*" => Star,
		// binary
		"+" => Plus,
		"/" => Div,
		"==" => Eq,
		"!=" => Ne,
		"<" => Lt,
		">" => Gt,
		"<=" => Le,
		">=" => Ge,
	}

	AssignmentOp {
		"=" => Set,
		"-=" => NegSet,
		"*=" => StarSet,
		"+=" => PlusSet,
		"/=" => DivSet,
	}

	Punctuation {
		"(" => LParen,
		")" => RParen,
		"[" => LBracket,
		"]" => RBracket,
		"{" => LBrace,
		"}" => RBrace,
		"," => Comma,
		";" => Semicolon,
	}
);

pub fn lex(code: &str, file_id: usize) -> Result<Vec<Spanned<Token>>, Vec<Diagnostic<usize>>> {
	let lex = Token::lexer(code).spanned();
	let tokens = lex
		.map(|(token, range)| (token, Span::new(file_id, range)))
		.collect::<Vec<Spanned<Token>>>();
	let mut diagnostics = vec![];
	for token in tokens.clone() {
		if token.0 == Token::Error {
			diagnostics.push(
				Diagnostic::error()
					.with_message("could not parse token")
					.with_labels(vec![Label::primary(token.1.file_id, token.1.range())
						.with_message("invalid token")]),
			)
		}
	}
	if !diagnostics.is_empty() {
		Err(diagnostics)
	} else {
		Ok(tokens)
	}
}
