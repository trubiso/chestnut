#[macro_export]
macro_rules! token_parser {
	($ty:ty) => {
		impl chumsky::Parser<$crate::lexer::Token, $ty, Error = chumsky::error::Simple<$crate::lexer::Token, $crate::span::Span>>
	};

	($ty:ty : '_) => {
		impl chumsky::Parser<$crate::lexer::Token, $ty, Error = chumsky::error::Simple<$crate::lexer::Token, $crate::span::Span>> + '_
	};
}

#[macro_export]
macro_rules! ident {
	($str:expr) => {
		Ident::Named($str.to_string())
	};
}

#[macro_export]
macro_rules! span {
	($x:expr) => {
		$x.map_with_span(|x, s| (x, s))
	};
}

macro_rules! token_gen {
	($name:ident, $jname:ident => $ident:ident) => {
		#[macro_export]
		macro_rules! $name {
			($var:ident) => {
				$crate::lexer::Token::$ident($crate::lexer::$ident::$var)
			};
		}
		#[macro_export]
		macro_rules! $jname {
			($var:ident) => {
				just($name!($var))
			};
		}
	};
}

token_gen!(keyword, jkeyword => Keyword);
token_gen!(punct, jpunct => Punctuation);
token_gen!(_assg_op, jassg_op => AssignmentOp);
token_gen!(op, jop => Operator);

macro_rules! delim_gen {
	($name:ident => ($mac:ident, $wmac:ident) + $l:ident, $r:ident) => {
		#[macro_export]
		macro_rules! $name {
			($arg:expr) => {
				$arg.delimited_by($wmac!($l), $wmac!($r))
			};
			($arg:expr, $sep:ident) => {
				$name!($arg.separated_by(jpunct!($sep)).allow_trailing())
			};
			($arg:expr,) => {
				$name!($arg, Comma)
			};
		}
	};
	($name:ident => $l:ident, $r:ident) => {
		delim_gen!($name => (punct, jpunct) + $l, $r);
	};
}

delim_gen!(parened => LParen, RParen);
delim_gen!(braced => LBrace, RBrace);
delim_gen!(bracketed => LBracket, RBracket);
delim_gen!(angled => (op, jop) + Lt, Gt);

#[macro_export]
macro_rules! builtin {
	($span:expr, $var:ident) => {
		Type::Builtin($span, BuiltinType::$var)
	};
}

#[macro_export]
macro_rules! force_token {
	($value:expr => Identifier, $span:expr) => {
		match $value {
			$crate::lexer::Token::Identifier(x) => $crate::parser::types::Ident::Named($span, x),
			_ => unreachable!(),
		}
	};
	($value:expr => $kind:ident) => {
		match $value {
			$crate::lexer::Token::$kind(x) => x,
			_ => unreachable!(),
		}
	};
	($value:expr => $kind:ident, $span:expr) => {
		force_token!($value => $kind)
	};
}

macro_rules! assg {
	($ident:ident) => {
		$crate::parser::ident::ident()
			.then(jassg_op!($ident))
			.then(expr())
	};
	(ignore $ident:ident) => {
		$crate::parser::ident::ident()
			.then_ignore(jassg_op!($ident))
			.then(expr())
	};
	(noident $ident:ident) => {
		jassg_op!($ident).then(expr())
	};
	(noident ignore $ident:ident) => {
		jassg_op!($ident).ignore_then(expr())
	};
}
