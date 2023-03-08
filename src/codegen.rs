use crate::{
	parser::types::{Expr, Privacy, Type, TypedIdent},
	resolve::{ResolvedScope, ResolvedStmt}, lexer::{NumberLiteralRepr, NumberLiteralKind},
};

pub fn codegen_expr(expr: Expr) -> String {
	match expr {
		Expr::CharLiteral(_, value) => value,
		Expr::StringLiteral(_, value) => value,
		Expr::NumberLiteral(_, number_literal) => {
			let prefix = match number_literal.repr {
				NumberLiteralRepr::Binary => "0b",
				NumberLiteralRepr::Octal => "0o",
				NumberLiteralRepr::Hex => "0x",
				NumberLiteralRepr::Decimal => ""
			};
			let value = number_literal.value;
			match number_literal.kind {
			NumberLiteralKind::IZ => format!("((iz){prefix}{value})"),
			NumberLiteralKind::UZ => format!("((uz){prefix}{value}u)"),
			x => format!("{prefix}{value}{x}"),
			}
		},
		Expr::Identifier(_, ident) => ident.to_string(),
		Expr::BinaryOp(_, lhs, op, rhs) => format!("({}{op}{})", codegen_expr(*lhs), codegen_expr(*rhs)),
		Expr::UnaryOp(_, op, val) => format!("({op}{})", codegen_expr(*val)),
		Expr::Lambda(_, func) => todo!(),
		Expr::Call(_, callee, args) => todo!(),
		Expr::Error(_) => panic!(),
	}
}

pub fn codegen_ty(ty: Type) -> String {
	fn inner(ty: Type, comes_from_mut: bool) -> String {
		let m = if comes_from_mut { "" } else { " const" };
		match ty {
			Type::BareType(_, x) => format!(
				"{}{}{m}",
				x.ident.to_string(),
				x.generics
					.iter()
					.map(|x| inner(x.clone(), false))
					.reduce(|acc, b| acc + ", " + &b)
					.map(|x| format!("<{x}>"))
					.unwrap_or_else(String::new)
			),
			Type::Builtin(_, x) => format!("{x}{m}"),
			Type::Array(_, lhs, rhs) => format!(
				"Chestnut::Array<{}{}>{m}",
				*lhs,
				rhs.map(|x| ",".to_string() + &codegen_expr(*x))
					.unwrap_or_else(String::new)
			),
			Type::Ref(_, x) => format!("Chestnut::Ref<{}>{m}", inner(*x, false)),
			Type::Optional(_, x) => format!("Chestnut::Optional<{}>{m}", inner(*x, false)),
			Type::Mut(_, x) => inner(*x, true),
			Type::Inferred(_) => panic!("unsupported Inferred type in codegen!"),
		}
	}
	inner(ty, false)
}

pub fn codegen_ty_ident(ty_ident: TypedIdent) -> String {
	format!("{} {}", codegen_ty(ty_ident.ty), ty_ident.ident.to_string())
}

pub fn codegen_privacy(privacy: Privacy) -> String {
	privacy.to_string()
}

pub fn codegen_stmt(stmt: ResolvedStmt) -> String {
	match stmt {
		ResolvedStmt::Create(_, privacy, ty_ident, expr) => {
			format!("{}{} = {};", codegen_privacy(privacy), codegen_ty_ident(ty_ident), codegen_expr(expr))
		}
		ResolvedStmt::Declare(_, privacy, ty_ident) => {
			format!("{}{};", codegen_privacy(privacy), codegen_ty_ident(ty_ident))
		}
		ResolvedStmt::Set(_, ident, expr) => {
			format!("{} = {};", ident.to_string(), codegen_expr(expr))
		}
		_ => "".into()
	}
}

pub fn codegen(scope: ResolvedScope) -> String {
	let mut code = "".to_string();
	for (_, mut func) in scope.funcs {
		code += &format!("[[nodiscard]] {} {}(", func.return_ty, func.name);
		for (i, arg) in func.args.iter().enumerate() {
			code += &format!("{} {}", arg.ty, arg.name);
			if i < func.args.len() - 1 {
				code += ", ";
			}
		}
		code += ") {";
		// FIXME: separate inherited and non-inherited funcs & classes, otherwise
		// our output becomes gigantic. this is just a workaround
		func.body.funcs.clear();
		code += &codegen(func.body);
		code += "}";
	}
	for stmt in scope.stmts {
		code += &codegen_stmt(stmt);
	}
	code
}
