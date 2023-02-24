#[macro_export]
macro_rules! ident {
	($str:expr) => {
		Ident::Named($str.to_string())
	};
}

macro_rules! token_gen {
	($name:ident, $jname:ident => $ident:ident) => {
		#[macro_export]
		macro_rules! $name {
			($var:ident) => {
				Token::$ident($ident::$var)
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

macro_rules! all_delim_tuples {
	() => {
		[
			(punct!(LBrace), punct!(RBrace)),
			(punct!(LBracket), punct!(RBracket)),
			(punct!(LParen), punct!(RParen)),
			(op!(Lt), op!(Gt)),
		]
	};
}

macro_rules! delim_gen {
	($name:ident => ($mac:ident, $wmac:ident) + $l:ident, $r:ident) => {
		#[macro_export]
		macro_rules! $name {
			($arg:expr; $recovery:expr) => {
				$arg.delimited_by($wmac!($l), $wmac!($r)).recover_with(
					nested_delimiters($mac!($l), $mac!($r), all_delim_tuples!(), $recovery)
				)
			};
			($arg:expr, $sep:ident; $recovery:expr) => {
				$name!($arg.separated_by(jpunct!($sep)).allow_trailing(); $recovery)
			};
			($arg:expr,; $recovery:expr) => {
				$name!($arg, Comma; $recovery)
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
	($value:expr => Identifier) => {
		match $value {
			Token::Identifier(x) => Ident::Named(x),
			_ => unreachable!(),
		}
	};
	($value:expr => $kind:ident) => {
		match $value {
			Token::$kind(x) => x,
			_ => unreachable!(),
		}
	};
}

#[macro_export]
macro_rules! assg {
	($ident:ident) => {
		ident()
			.then(jassg_op!($ident))
			.then(expr())
	};
	(ignore $ident:ident) => {
		ident()
			.then_ignore(jassg_op!($ident))
			.then(expr())
	};
	(noident $ident:ident) => {
		jassg_op!($ident)
			.then(expr())
	};
	(noident ignore $ident:ident) => {
		jassg_op!($ident)
			.ignore_then(expr())
	};
}

#[macro_export]
macro_rules! assg_stmt {
	($ident:ident) => {
		assg!($ident).map(|((lhs, _), rhs)| Stmt::Set(lhs, rhs))
	};
	($ident:ident => $op:ident) => {
		assg!($ident).map(|((lhs, _), rhs)| {
			Stmt::Set(
				lhs.clone(),
				Expr::BinaryOp(
					Box::new(Expr::Identifier(lhs.clone())),
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
				choice(($(jop!($op),)*))
				.then($next()).repeated())
			.foldl(|lhs, (op, rhs)| Expr::BinaryOp(Box::new(lhs), force_token!(op => Operator), Box::new(rhs)))
	};
}

#[macro_export]
macro_rules! unop_parser {
	($($op:ident)* => $next:ident) => {
		|| choice(($(jop!($op),)*)).repeated()
			.then($next())
			.foldr(|op, rhs| Expr::UnaryOp(force_token!(op => Operator), Box::new(rhs)))
	};
}

#[macro_export]
macro_rules! literal_parser {
	($kind:ident) => {
		filter(|x| matches!(x, Token::$kind(_))).map(|x| {
			Expr::$kind(force_token!(x => $kind))
		})
	};
}
