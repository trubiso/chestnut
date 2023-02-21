use codespan_reporting::diagnostic::{Diagnostic, Label};
use logos::{Logos, Span};
use std::fmt::Display;

#[derive(Logos, Debug, PartialEq, Eq, Clone, Hash)]
pub enum Token {
	#[error]
	#[regex(r"\s+", logos::skip)]
	Error,
	// (i(?:z|8|16|32|64|128)|u(?:z|8|16|32|64|128)?|f(?:16|32|64|128))? matches int suffixes
	// integers
	#[regex(r"[0-9][0-9_]*(i(?:z|8|16|32|64|128)|u(?:z|8|16|32|64|128)?|f(?:16|32|64|128))?", |x| x.slice().trim().trim().to_string())]
	// floats (only f16-f128 obv)
	#[regex(r"(?:[0-9][0-9_]*)?\.[0-9][0-9_]*(f(?:16|32|64|128))?", |x| x.slice().trim().to_string())]
	// binary
	#[regex(r"0b[01][01_]*(i(?:z|8|16|32|64|128)|u(?:z|8|16|32|64|128)?|f(?:16|32|64|128))?", |x| x.slice().trim().to_string())]
	// octal
	#[regex(r"0o[0-7][0-7_]*(i(?:z|8|16|32|64|128)|u(?:z|8|16|32|64|128)?|f(?:16|32|64|128))?", |x| x.slice().trim().to_string())]
	// hex
	#[regex(r"0x[0-9a-fA-F][0-9a-fA-F_]*(i(?:z|8|16|32|64|128)|u(?:z|8|16|32|64|128)?|f(?:16|32|64|128))?", |x| x.slice().trim().to_string())]
	// TODO: use NumberLiteralToken
	NumberLiteral(String),
	#[regex(r"'.'", |x| x.slice().trim().to_string())]
	CharLiteral(String),
	#[regex(r#""(?:[^"]|\\")*""#, |x| x.slice().trim().to_string())]
	StringLiteral(String),
	#[token("public", |_| Keyword::Public)]
	#[token("func", |_| Keyword::Function)]
	#[token("return", |_| Keyword::Return)]
	#[token("->", |_| Keyword::Arrow)]
	#[token("=>", |_| Keyword::FatArrow)]
	#[token("let", |_| Keyword::Let)]
	#[token("const", |_| Keyword::Const)]
	#[token("for", |_| Keyword::For)]
	#[token("in", |_| Keyword::In)]
	#[token("while", |_| Keyword::While)]
	#[token("loop", |_| Keyword::Loop)]
	#[token("if", |_| Keyword::If)]
	Keyword(Keyword),
	#[token("?", |_| Operator::Question)]
	#[token("!", |_| Operator::Bang)]
	#[token("&", |_| Operator::Amp)]
	#[token("&&", |_| Operator::And)]
	#[token("||", |_| Operator::Or)]
	// unary/binary
	#[token("-", |_| Operator::Neg)]
	#[token("*", |_| Operator::Star)]
	// binary
	#[token("+", |_| Operator::Plus)]
	#[token("/", |_| Operator::Div)]
	#[token("==", |_| Operator::Eq)]
	#[token("!=", |_| Operator::Ne)]
	#[token("<", |_| Operator::Lt)]
	#[token(">", |_| Operator::Gt)]
	#[token("<=", |_| Operator::Le)]
	#[token(">=", |_| Operator::Ge)]
	Operator(Operator),
	#[token("=", |_| AssignmentOp::Set)]
	#[token("-=", |_| AssignmentOp::NegSet)]
	#[token("*=", |_| AssignmentOp::StarSet)]
	#[token("+=", |_| AssignmentOp::PlusSet)]
	#[token("/=", |_| AssignmentOp::DivSet)]
	AssignmentOp(AssignmentOp),
	#[token("(", |_| Punctuation::LParen)]
	#[token(")", |_| Punctuation::RParen)]
	#[token("[", |_| Punctuation::LBracket)]
	#[token("]", |_| Punctuation::RBracket)]
	#[token("{", |_| Punctuation::LBrace)]
	#[token("}", |_| Punctuation::RBrace)]
	#[token(",", |_| Punctuation::Comma)]
	#[token(";", |_| Punctuation::Semicolon)]
	Punctuation(Punctuation),
	#[regex(r"[a-zA-Z_][a-zA-Z0-9_]*", |x| x.slice().trim().to_string())]
	Identifier(String),
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub enum Operator {
	Question,
	Bang,
	Amp,
	And,
	Or,
	Neg,
	Star,
	Plus,
	Div,
	Eq,
	Ne,
	Lt,
	Gt,
	Le,
	Ge,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub enum AssignmentOp {
	Set,
	NegSet,
	StarSet,
	PlusSet,
	DivSet,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub enum Punctuation {
	LParen,
	RParen,
	LBracket,
	RBracket,
	LBrace,
	RBrace,
	Comma,
	Semicolon,
}

impl Display for Operator {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		f.write_str(match *self {
			Self::Question => "?",
			Self::Bang => "!",
			Self::Amp => "&",
			Self::And => "&&",
			Self::Or => "||",
			Self::Neg => "-",
			Self::Star => "*",
			Self::Plus => "+",
			Self::Div => "/",
			Self::Eq => "==",
			Self::Ne => "!=",
			Self::Lt => "<",
			Self::Gt => ">",
			Self::Le => "<=",
			Self::Ge => ">=",
		})
	}
}

impl Display for AssignmentOp {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		f.write_str(match *self {
			Self::Set => "=",
			Self::NegSet => "-=",
			Self::StarSet => "*=",
			Self::PlusSet => "+=",
			Self::DivSet => "/=",
		})
	}
}

impl Display for Punctuation {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		f.write_str(match *self {
			Self::LParen => "(",
			Self::RParen => ")",
			Self::LBracket => "[",
			Self::RBracket => "]",
			Self::LBrace => "{",
			Self::RBrace => "}",
			Self::Comma => ",",
			Self::Semicolon => ";",
		})
	}
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub enum Keyword {
	Public,
	Function,
	Return,
	Arrow,
	FatArrow,
	Let,
	Const,
	For,
	In,
	While,
	Loop,
	If,
}

impl Display for Keyword {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		f.write_str(match *self {
			Self::Public => "public",
			Self::Function => "func",
			Self::Return => "return",
			Self::Arrow => "->",
			Self::FatArrow => "=>",
			Self::Let => "let",
			Self::Const => "const",
			Self::For => "for",
			Self::In => "in",
			Self::While => "while",
			Self::Loop => "loop",
			Self::If => "if",
		})
	}
}

pub type Spanned<T> = (T, Span);

pub fn lex(code: &str, file_id: usize) -> Result<Vec<Spanned<Token>>, Vec<Diagnostic<usize>>> {
	let lex = Token::lexer(code).spanned();
	let tokens = lex.collect::<Vec<Spanned<Token>>>();
	let mut diagnostics = vec![];
	for token in tokens.clone() {
		if token.0 == Token::Error {
			diagnostics.push(
				Diagnostic::error()
					.with_message("could not parse token")
					.with_labels(vec![
						Label::primary(file_id, token.1).with_message("invalid token")
					]),
			)
		}
	}
	if !diagnostics.is_empty() {
		Err(diagnostics)
	} else {
		Ok(tokens)
	}
}

// #[derive(Debug, PartialEq, Eq)]
// pub struct NumberLiteralToken {
// 	pub repr: NumberLiteralTokenRepr,
// 	pub kind: NumberLiteralTokenKind,
// 	pub value: String,
// }

// #[derive(Debug, PartialEq, Eq)]
// pub enum NumberLiteralTokenRepr {
// 	Binary,
// 	Octal,
// 	Hex,
// 	Decimal,
// }

// #[derive(Debug, PartialEq, Eq)]
// pub enum NumberLiteralTokenKind {
// 	I8,
// 	I16,
// 	I32,
// 	I64,
// 	I128,
// 	Z,
// 	U8,
// 	U16,
// 	U32,
// 	U64,
// 	U128,
// 	UZ,
// 	F16,
// 	F32,
// 	F64,
// 	F128,
// }
