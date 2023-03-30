use std::{iter::Peekable, str::Chars};

use crate::span::Span;
use codespan_reporting::diagnostic::{Diagnostic, Label};
use derive_more::Display;

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

pub struct Lexer<'a> {
	data: Peekable<Chars<'a>>,
	cursor: usize,
	file_id: usize,
}

#[derive(Display)]
pub enum LexError {
	/// Unexpected end of file
	#[display(fmt = "unexpected end of file")]
	UnexpectedEOF,
	/// Unexpected start of file (this should probably never happen)
	#[display(fmt = "unexpected start of file")]
	UnexpectedSOF,
	/// Found a character that didn't match any token rules
	#[display(fmt = "unknown token")]
	UnknownToken,
	/// Found a non-existing escape sequence in a char/string literal, or simply
	/// an invalid one (e.g. unicode codepoints with more than 5 hex digits)
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

// basic lex functions
impl Lexer<'_> {
	/// Returns the current character, returning ' ' if the end was reached.
	fn peek(&mut self) -> char {
		self.data.peek().map(|x| *x).unwrap_or('\u{fffff}')
	}

	/// Returns the current character, and advances the cursor past it. If the
	/// cursor points more than 1 past the end of the data (i.e. past EOF),
	/// LexError::UnexpectedEOF will be returned instead.
	fn consume(&mut self) -> Result<char, LexError> {
		let data = self.peek();
		self.advance()?;
		Ok(data)
	}

	/// Returns whether the cursor points past the end of the data. The cursor
	/// may point 1 past the end to signal that it has finished lexing, but
	/// pointing further than this only happens when the end wasn't expected.
	fn eof(&mut self) -> bool {
		self.data.peek().is_none()
	}

	fn next_eof(&self) -> bool {
		let mut cloned = self.data.clone();
		cloned.next();
		cloned.peek().is_none()
	}

	/// Advances the cursor once. Returns LexError::UnexpectedEOF if the cursor
	/// points past EOF (1 past the end).
	fn advance(&mut self) -> Result<(), LexError> {
		self.cursor += 1;
		self.data.next();
		if self.next_eof() {
			Err(LexError::UnexpectedEOF)
		} else {
			Ok(())
		}
	}

	/// Moves the cursor back once. Returns LexError::UnexpectedEOS if this
	/// movement would imply overflowing the usize back to its maximum, although
	/// this situation is highly unlikely.
	fn backtrace(&mut self) -> Result<(), LexError> {
		if self.cursor == 0 {
			return Err(LexError::UnexpectedSOF);
		}
		self.cursor -= 1;
		Ok(())
	}

	/// Skips whitespace characters as long as EOF isn't reached (hence this
	/// function should never error).
	fn skip_whitespace(&mut self) {
		while !self.eof() && is_whitespace(self.peek()) {
			_ = self.advance();
		}
	}

	/// Skips until the specified delimiter.
	fn skip_until(&mut self, delim: char) -> Result<(), LexError> {
		while self.peek() != delim {
			self.advance()?;
		}
		Ok(())
	}
}

// make tokens and spans
impl Lexer<'_> {
	fn span(&self, n: usize) -> Span {
		Span::new(self.file_id, self.cursor - n..self.cursor)
	}
}

macro_rules! t {
	($s:expr, $v:ident $(| $c:expr => $d:ident)*) => {
		$(if $s.peek() == $c {
			$s.advance()?;
			Ok(Some(Token::$d($s.span(2))))
		} else)* {
			Ok(Some(Token::$v($s.span(1))))
		}
	};
}

fn is_whitespace(x: char) -> bool {
	match x {
		'\t' | '\n' | '\r' | ' ' | '\0' => true,
		_ => false,
	}
}

/// Returns whether a character could be parsed into a token, excluding
/// identifiers.
fn is_lex_hint(x: char) -> bool {
	match x {
		'?' | '~' | '*' | '+' | '(' | ')' | '[' | ']' | '{' | '}' | ',' | ';' | '&' | '|' | '.'
		| ':' | '!' | '/' | '=' | '<' | '>' | '"' | '\'' => true,
		_ => false,
	}
}

/// Returns whether a character can be taken in by the lexer, excluding
/// identifiers.
fn is_lexable(x: char) -> bool {
	is_whitespace(x) || is_lex_hint(x)
}

// actual lexing
impl Lexer<'_> {
	pub fn new(data: Peekable<Chars<'_>>, file_id: usize) -> Lexer {
		Lexer {
			data,
			cursor: 0,
			file_id,
		}
	}

	fn slash(&mut self) -> Result<Option<Token>, LexError> {
		match self.peek() {
			'/' => {
				self.skip_until('\n')?;
				Ok(None)
			}
			'*' => {
				// TODO: use LexError::UnclosedBlockComment
				// skip *
				self.advance()?;
				// we are now inside the comment
				loop {
					// TODO: nested block comments
					// look for closing *
					self.skip_until('*')?;
					// skip it
					self.advance()?;
					// then if we find a / we can end
					if self.peek() == '/' {
						self.advance()?;
						break;
					}
				}
				Ok(None)
			}
			_ => Ok(Some(Token::Div(self.span(1)))),
		}
	}

	fn number_literal(&mut self) -> Result<Option<Token>, LexError> {
		todo!()
	}

	fn char_literal(&mut self) -> Result<Option<Token>, LexError> {
		todo!()
	}

	fn string_literal(&mut self) -> Result<Option<Token>, LexError> {
		todo!()
	}

	fn skip_until_lexable(&mut self) -> Result<String, LexError> {
		let mut skipped = "".to_string();
		while !is_lexable(self.peek()) {
			skipped += &self.consume()?.to_string();
		}
		return Ok(skipped);
	}

	fn ident(&mut self) -> Result<Option<Token>, LexError> {
		self.backtrace()?;
		let data = self.skip_until_lexable()?;
		Ok(Some(Token::Ident(self.span(data.len()), data)))
	}

	pub fn lex_one(&mut self) -> Result<Option<Token>, LexError> {
		// this is the best Rust code i have written to date
		// (the block comments stop rustfmt from breaking my alignment)
		let current = self.consume()?;
		match current {
			'?' => t!(self, Question /* /*                       */ */),
			'~' => t!(self, Tilde /* /*                          */ */),
			'*' => t!(self, Star /* /*                           */ */),
			'+' => t!(self, Plus /* /*                           */ */),
			'(' => t!(self, LParen /* /*                         */ */),
			')' => t!(self, RParen /* /*                         */ */),
			'[' => t!(self, LSquare /* /*                        */ */),
			']' => t!(self, RSquare /* /*                        */ */),
			'{' => t!(self, LCurly /* /*                         */ */),
			'}' => t!(self, RCurly /* /*                         */ */),
			',' => t!(self, Comma /* /*                          */ */),
			';' => t!(self, Semi /* /*                           */ */),
			'&' => t!(self, Amp      | '&' => DAmp                    ),
			'|' => t!(self, Pipe     | '|' => DPipe                   ),
			'.' => t!(self, Dot      | '.' => DDot                    ),
			':' => t!(self, Colon    | ':' => DColon                  ),
			'-' => t!(self, Dash     | '>' => Arrow                   ),
			'!' => t!(self, Bang     | '=' => Ne                      ),
			'<' => t!(self, Lt       | '=' => Le                      ),
			'>' => t!(self, Gt       | '=' => Ge                      ),
			'=' => t!(self, Set      | '=' => Eq     | '>' => FatArrow),

			'/' => self.slash(),

			'0'..='9' => self.number_literal(),

			'\'' => self.char_literal(),

			'"' => self.string_literal(),

			'a'..='z' | 'A'..='Z' | '_' => self.ident(),

			_ => Err(LexError::UnknownToken),
		}
	}

	pub fn lex(&mut self) -> (Vec<Token>, Vec<Diagnostic<usize>>) {
		let mut tokens = vec![];
		let mut diagnostics = vec![];
		loop {
			self.skip_whitespace();
			if self.eof() {
				break;
			}
			let previous = self.cursor;
			match self.lex_one() {
				Ok(token) => match token {
					Some(token) => tokens.push(token),
					None => {}
				},
				Err(error) => diagnostics.push(
					Diagnostic::error()
						.with_message(format!("{error}"))
						.with_labels(vec![Label::primary(self.file_id, previous..self.cursor)]),
				),
			}
		}
		(tokens, diagnostics)
	}
}

pub fn lex(data: &str, file_id: usize) -> (Vec<Token>, Vec<Diagnostic<usize>>) {
	let mut lexer = Lexer {
		data: data.chars().peekable(),
		cursor: 0,
		file_id,
	};

	lexer.lex()
}
