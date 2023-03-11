use crate::span::{Span, Spanned};
use codespan_reporting::diagnostic::{Diagnostic, Label};
use derive_more::Display;
use logos::{Lexer, Logos};
use regex::Regex;
use std::fmt::Display;

fn lex_to_str(lexer: &Lexer<'_, Token>) -> String {
	lexer.slice().trim().to_string()
}

fn parse_number_literal(lexer: &Lexer<'_, Token>) -> NumberLiteral {
	let number_str = lex_to_str(lexer);
	let suffix =
		Regex::new(r"i(?:z|8|16|32|64|128)|u(?:z|8|16|32|64|128)?|f(?:16|32|64|128)?").unwrap();
	let hex_suffix =
		Regex::new(r"i(?:z|8|16|32|64|128)|u(?:z|8|16|32|64|128)?|p(?:16|32|64|128)?").unwrap();
	let bin = Regex::new(r"0b[01][01_]*").unwrap();
	let oct = Regex::new(r"0o[0-7][0-7_]*").unwrap();
	let hex = Regex::new(r"0x[0-9a-fA-F][0-9a-fA-F_]*").unwrap();
	let flt = Regex::new(r"(?:[0-9][0-9_]*)?\.[0-9][0-9_]*").unwrap();
	let int = Regex::new(r"[0-9][0-9_]*").unwrap();
	let which_one_parses = |string: &str| {
		if bin.is_match(string) {
			NumberLiteralRepr::Binary
		} else if oct.is_match(string) {
			NumberLiteralRepr::Octal
		} else if hex.is_match(string) {
			NumberLiteralRepr::Hex
		} else if flt.is_match(string) || int.is_match(string) {
			NumberLiteralRepr::Decimal
		} else {
			unreachable!()
		}
	};
	let repr = which_one_parses(&number_str);
	let suf = if repr == NumberLiteralRepr::Hex {
		hex_suffix
	} else {
		suffix
	}
	.find(&number_str);
	let kind = if let Some(suf) = suf {
		match suf.as_str() {
			"i8" => NumberLiteralKind::I8,
			"i16" => NumberLiteralKind::I16,
			"i32" => NumberLiteralKind::I32,
			"i64" => NumberLiteralKind::I64,
			"i128" => NumberLiteralKind::I128,
			"iz" => NumberLiteralKind::IZ,
			"u" => NumberLiteralKind::U,
			"u8" => NumberLiteralKind::U8,
			"u16" => NumberLiteralKind::U16,
			"u32" => NumberLiteralKind::U32,
			"u64" => NumberLiteralKind::U64,
			"u128" => NumberLiteralKind::U128,
			"uz" => NumberLiteralKind::UZ,
			"f" | "p" => NumberLiteralKind::F,
			"f16" | "p16" => NumberLiteralKind::F16,
			"f32" | "p32" => NumberLiteralKind::F32,
			"f64" | "p64" => NumberLiteralKind::F64,
			"f128" | "fp28" => NumberLiteralKind::F128,
			_ => unreachable!("{suf:?}"),
		}
	} else {
		NumberLiteralKind::None
	};

	let actual_number = if let Some(suf) = suf {
		&number_str[..suf.range().start]
	} else {
		&number_str
	};
	let actual_number = Regex::new(r"0(?:b|o|x)")
		.unwrap()
		.replace_all(actual_number, "")
		.to_string();
	let actual_number = actual_number.replace('_', "");

	NumberLiteral {
		repr,
		kind,
		value: actual_number,
	}
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct NumberLiteral {
	pub repr: NumberLiteralRepr,
	pub kind: NumberLiteralKind,
	pub value: String,
}

impl Display for NumberLiteral {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		f.write_fmt(format_args!("{}{}{}", self.repr, self.value, self.kind))
	}
}

#[derive(Debug, derive_more::Display, PartialEq, Eq, Clone, Copy, Hash)]
pub enum NumberLiteralRepr {
	#[display(fmt = "b")]
	Binary,
	#[display(fmt = "o")]
	Octal,
	#[display(fmt = "x")]
	Hex,
	#[display(fmt = "")]
	Decimal,
}

#[derive(Debug, derive_more::Display, PartialEq, Eq, Clone, Copy, Hash)]
pub enum NumberLiteralKind {
	#[display(fmt = "")]
	None,
	I8,
	I16,
	I32,
	I64,
	I128,
	IZ,
	U,
	U8,
	U16,
	U32,
	U64,
	U128,
	UZ,
	F,
	F16,
	F32,
	F64,
	F128,
}

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
			#[regex(r"//[^\n]*", logos::skip)]
			#[regex(r"/\*(?:[^*]|\*[^/])*\*/", logos::skip)]
			Error,
			#[regex(r"(?:([0-9][0-9_]*|(?:[0-9][0-9_]*)?\.[0-9][0-9_]*|0b[01][01_]*|0o[0-7][0-7_]*)(i(?:z|8|16|32|64|128)|u(?:z|8|16|32|64|128)?|f(?:16|32|64|128)?)?|(0x[0-9a-fA-F][0-9a-fA-F_]*)(i(?:z|8|16|32|64|128)|u(?:z|8|16|32|64|128)?|p(?:16|32|64|128)?)?)", parse_number_literal)]
			NumberLiteral(NumberLiteral),
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
		"private" => Private,
		"protected" => Protected,
		"public" => Public,
		"export" => Export,
		"class" => Class,
		"func" => Function,
		"pure" => Pure,
		"return" => Return,
		"->" => Arrow,
		"=>" => FatArrow,
		"let" => Let,
		"mut" => Mut,
		"for" => For,
		"in" => In,
		"while" => While,
		"loop" => Loop,
		"if" => If,
		"import" => Import,
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
		":" => Colon,
		"::" => ColonColon,
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
