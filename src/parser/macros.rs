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
	($var:ident) => {
		Type::Builtin(BuiltinType::$var)
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
			Token::$kind(x) => x,
			_ => unreachable!(),
		}
	};
	($value:expr => $kind:ident, $span:expr) => {
		force_token!($value => $kind)
	};
}

#[macro_export]
macro_rules! assg {
	($ident:ident) => {
		$crate::parser::ident::ident().then(jassg_op!($ident)).then(expr())
	};
	(ignore $ident:ident) => {
		$crate::parser::ident::ident().then_ignore(jassg_op!($ident)).then(expr())
	};
	(noident $ident:ident) => {
		jassg_op!($ident).then(expr())
	};
	(noident ignore $ident:ident) => {
		jassg_op!($ident).ignore_then(expr())
	};
}

#[macro_export]
macro_rules! assg_stmt {
	($ident:ident) => {
		assg!($ident).map_with_span(|((lhs, _), rhs), span| Stmt::Set(span, lhs, rhs))
	};
	($ident:ident => $op:ident) => {
		assg!($ident).map_with_span(|((lhs, _), rhs), span| {
			Stmt::Set(
				span.clone(),
				lhs.clone(),
				Expr::BinaryOp(
					span,
					Box::new(Expr::Identifier(lhs.clone().span(), lhs)),
					Operator::$op,
					Box::new(rhs),
				),
			)
		})
	};
}

#[macro_export]
macro_rules! binop_parser {
	($($op:ident)* => $next:ident) => {
		|| $next()
			.then(
				span!(choice(($(jop!($op),)*)))
				.then($next()).repeated())
				// FIXME: get the proper span of lhs + op + rhs
			.foldl(|lhs, ((op, span), rhs)| Expr::BinaryOp(span, Box::new(lhs), force_token!(op => Operator), Box::new(rhs)))
	};
}

#[macro_export]
macro_rules! unop_parser {
	($($op:ident)* => $next:ident) => {
		|| span!(choice(($(jop!($op),)*))).repeated()
			.then($next())
			// NOTE: i don't know if this span is correct?
			// TODO: perhaps get rhs span
			.foldr(|(op, s), rhs| Expr::UnaryOp(s, force_token!(op => Operator), Box::new(rhs)))
	};
}

#[macro_export]
macro_rules! literal_parser {
	($kind:ident) => {
		filter(|x| matches!(x, Token::$kind(_))).map_with_span(|x, span: Span| {
			Expr::$kind(span.clone(), force_token!(x => $kind, span))
		})
	};
}
