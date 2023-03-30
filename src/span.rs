use std::fmt::Display;

pub type Spanned<T> = (T, Span);

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Span {
	pub file_id: usize,
	pub start: usize,
	pub end: usize,
}

impl Span {
	pub fn new(file_id: usize, range: std::ops::Range<usize>) -> Self {
		Self {
			file_id,
			start: range.start,
			end: range.end,
		}
	}

	pub fn range(&self) -> std::ops::Range<usize> {
		self.start..self.end
	}
}

impl std::ops::Add for Span {
	type Output = Span;

	fn add(self, rhs: Self) -> Self::Output {
		assert!(self.file_id == rhs.file_id);
		Self {
			file_id: self.file_id,
			start: if rhs.start < self.start {
				rhs.start
			} else {
				self.start
			},
			end: if rhs.end > self.end {
				rhs.end
			} else {
				self.end
			},
		}
	}
}

impl Display for Span {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		f.write_fmt(format_args!(
			"file id {} @ {}..{}",
			self.file_id, self.start, self.end
		))
	}
}

impl chumsky::Span for Span {
	type Context = usize; // file id
	type Offset = usize; // start

	fn new(context: Self::Context, range: std::ops::Range<Self::Offset>) -> Self {
		Self {
			file_id: context,
			start: range.start,
			end: range.end,
		}
	}

	fn context(&self) -> Self::Context {
		self.file_id
	}

	fn start(&self) -> Self::Offset {
		self.start
	}

	fn end(&self) -> Self::Offset {
		self.end
	}
}

pub trait IntoSpan {
	fn span(&self) -> Span;
}
