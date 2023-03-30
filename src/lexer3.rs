use crate::span::Span;
use codespan_reporting::diagnostic::Diagnostic;
use derive_more::Display;
use std::str::Chars;

macro_rules! Token {
	($($ident:ident$(($($field:ty),*))?,)*) => {
		pub enum Token {
			$($ident(Span$($(,$field)*)?),)*
		}

		impl $crate::span::IntoSpan for Token {
			fn span(&self) -> Span {
				match self {
					$(Token::$ident(x, ..) => x,)*
				}.clone()
			}
		}
	}
}

Token! {
	// literals
	NumberLiteral(String), // number literal
	CharLiteral(String),   // char literal
	StringLiteral(String), // string literal
	// operator
	Question, // ?
	Bang,     // !
	Amp,      // &
	DAmp,     // &&
	Pipe,     // |
	DPipe,    // ||
	Dash,     // -
	Tilde,    // ~
	Star,     // *
	Plus,     // +
	Div,      // /
	Set,      // =
	Eq,       // ==
	Ne,       // !=
	Lt,       // <
	Gt,       // >
	Le,       // <=
	Ge,       // >=
	// punctuation
	LParen,   // (
	LSquare,  // [
	LCurly,   // {
	RParen,   // )
	RSquare,  // ]
	RCurly,   // }
	Dot,      // .
	DDot,     // ..
	Comma,    // ,
	Colon,    // :
	DColon,   // ::
	Semi,     // ;
	Arrow,    // ->
	FatArrow, // =>
	// misc
	Ident(String), // anything starting with a-zA-Z
}

#[derive(Display)]
pub enum LexError {
	/// Unexpected end of file.
	#[display(fmt = "unexpected end of file")]
	UnexpectedEOF,
	/// Found a character that didn't match any token rules.
	#[display(fmt = "unknown token")]
	UnknownToken,
	/// Found a non-existing escape sequence in a char/string literal, or simply
	/// an invalid one (e.g. unicode codepoints with more than 5 hex digits).
	#[display(fmt = "invalid escape sequence")]
	InvalidEscapeSequence,
	/// Found a char literal that does not have a matching closing quote.
	#[display(fmt = "unclosed char literal")]
	UnclosedCharLiteral,
	/// Found EOF while parsing a string literal.
	#[display(fmt = "unclosed string literal")]
	UnclosedStringLiteral,
	/// Found EOF while parsing a block comment.
	#[display(fmt = "unclosed block comment")]
	UnclosedBlockComment,
}

pub struct Lexer<'a> {
	input: Chars<'a>,
	cursor: usize,
	file_id: usize,
}

macro_rules! t {
	($self:expr, $($c:expr => $v:ident,)*) => {
		// dw about perf until its a genuine problem it will make your life easier
		// trust me i learned the hard way
		$(
			if $self.input.as_str().starts_with($c) {
                let len = $c.len();
				_ = $self.input.advance_by(len);
				$self.cursor += len;
                // advance len
				return Ok(Some(Token::$v($self.span(len))));
			}
		)*
	};
}

type Output = Result<Option<Token>, LexError>;

impl Lexer<'_> {
	pub fn span(&self, length: usize) -> Span {
		Span {
			file_id: self.file_id,
			start: self.cursor - length,
			end: self.cursor,
		}
	}

	pub fn slash(&mut self) -> Output {
		self.input.next();
		self.cursor += 2;
		match self.input.next().unwrap() {
			'/' => {
				// self.skip_until('\n');
				todo!()
			},
			_ => todo!()
		}
	}

	pub fn number_literal(&mut self) -> Output {
		todo!()
	}

	pub fn char_literal(&mut self) -> Output {
		todo!()
	}

	pub fn string_literal(&mut self) -> Output {
		todo!()
	}

	pub fn ident(&mut self) -> Output {
		todo!()
	}

	pub fn next(&mut self) -> Output {
		// should i move over macro t!
		//let tok = self.input.next() else { return Err(LexError::UnexpectedEOF) };

		// skip whitespace
		while let Some(c) = self.input.next() {
			if !c.is_whitespace() {
				break;
			}
		}

		if self.input.as_str().is_empty() {
			return Ok(None);
		}

		t!(self,
			"?" => Question,
			"~" => Tilde,
			"*" => Star,
			"+" => Plus,
			"(" => LParen,
			")" => RParen,
			"[" => LSquare,
			"]" => RSquare,
			"{" => LCurly,
			"}" => RCurly,
			"," => Comma,
			";" => Semi,
			"&&" => DAmp,
			"&" => Amp,
			"||" => DPipe,
			"|" => Pipe,
			".." => DDot,
			"." => Dot,
			"::" => DColon,
			":" => Colon,
			"->" => Arrow,
			"-" => Dash,
			"!=" => Ne,
			"!" => Bang,
			"<=" => Le,
			"<" => Lt,
			">=" => Ge,
			">" => Gt,
			"=>" => FatArrow,
			"==" => Eq,
			"=" => Set,
		);

		match self.input.clone().next().unwrap() {
			'/' => self.slash(),

			'0'..='9' => self.number_literal(),

			'\'' => self.char_literal(),

			'"' => self.string_literal(),

			'a'..='z' | 'A'..='Z' | '_' => self.ident(),

			_ => Err(LexError::UnknownToken),
		}
	}
}

pub fn lex(data: &str, file_id: usize) -> (Vec<Token>, Vec<Diagnostic<usize>>) {
	let mut lexer = Lexer {
		input: data.chars(),
		cursor: 0,
		file_id,
	};

	let mut tokens = vec![];
	let diagnostics = vec![];

	loop {
		match lexer.next() {
			Ok(x) => match x {
				Some(x) => tokens.push(x),
				None => break,
			},
			Err(_) => {}
		}
	}

	(tokens, diagnostics)
}
