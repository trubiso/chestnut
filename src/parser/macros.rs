#[macro_export]
macro_rules! ident {
	($str:expr) => {
		Ident($str.to_string())
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
				just(Token::$ident($ident::$var))
			};
		}
	};
}

token_gen!(_keyword, jkeyword => Keyword);
token_gen!(_punct, jpunct => Punctuation);
token_gen!(_assg_op, jassg_op => AssignmentOp);
token_gen!(_op, jop => Operator);

#[macro_export]
macro_rules! builtin {
	($var:ident) => {
		Type::Builtin(BuiltinType::$var)
	};
}

#[macro_export]
macro_rules! force_token {
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
			.then_ignore(jpunct!(Semicolon))
	};
	(ignore $ident:ident) => {
		ident()
			.then_ignore(jassg_op!($ident))
			.then(expr())
			.then_ignore(jpunct!(Semicolon))
	};
	(noident $ident:ident) => {
		jassg_op!($ident)
			.then(expr())
			.then_ignore(jpunct!(Semicolon))
	};
	(noident ignore $ident:ident) => {
		jassg_op!($ident)
			.ignore_then(expr())
			.then_ignore(jpunct!(Semicolon))
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
