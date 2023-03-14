use crate::{
	common::Privacy,
	lexer::{NumberLiteralKind, NumberLiteralRepr},
	parser::types::Ident,
	resolve::{
		ResolvedExpr, ResolvedFunc, ResolvedScope, ResolvedStmt, ResolvedType, ResolvedTypedIdent,
	},
};

// TODO: fix this disaster

pub fn comma<T>(args: Vec<T>, closure: fn(&T) -> String) -> String {
	args.iter()
		.map(closure)
		.reduce(|acc, b| acc + "," + &b)
		.unwrap_or_default()
}

pub fn codegen_mangle(str: &str) -> String {
	let mangle_prefix = "Chn_".to_owned();
	mangle_prefix + str
}

pub fn codegen_ident(ident: Ident) -> String {
	codegen_mangle(&ident.to_string())
}

pub fn codegen_expr(expr: ResolvedExpr) -> String {
	match expr {
		ResolvedExpr::CharLiteral(_, value) => value,
		ResolvedExpr::StringLiteral(_, value) => value,
		ResolvedExpr::NumberLiteral(_, number_literal) => {
			let prefix = match number_literal.repr {
				NumberLiteralRepr::Binary => "0b",
				NumberLiteralRepr::Octal => "0o",
				NumberLiteralRepr::Hex => "0x",
				NumberLiteralRepr::Decimal => "",
			};
			let value = number_literal.value;
			match number_literal.kind {
				NumberLiteralKind::IZ => format!("((iz){prefix}{value})"),
				NumberLiteralKind::UZ => format!("((uz){prefix}{value}u)"),
				x => format!("{prefix}{value}{x}"),
			}
		}
		ResolvedExpr::Identifier(_, ident) => codegen_ident(ident),
		ResolvedExpr::BinaryOp(_, lhs, op, rhs) => {
			format!("({}{op}{})", codegen_expr(*lhs), codegen_expr(*rhs))
		}
		ResolvedExpr::UnaryOp(_, op, val) => format!("({op}{})", codegen_expr(*val)),
		ResolvedExpr::Lambda(_, func) => {
			format!(
				"([&]({}){{{}}})",
				comma(func.args, |x| codegen_ty_ident(x.clone())),
				codegen_scope(func.body),
			)
		}
		// TODO: codegen generics
		ResolvedExpr::Call(_, callee, generics, args) => format!(
			"{}{}({})",
			codegen_expr(*callee),
			generics
				.map(|x| x.iter().map(|y| codegen_ty(y.clone())).collect::<String>())
				.map(|x| "<".to_string() + &x + ">")
				.unwrap_or("".to_string()),
			comma(args, |x| codegen_expr(x.clone()))
		),
		ResolvedExpr::Dot(..) => "".into(),
	}
}

pub fn codegen_ty(ty: ResolvedType) -> String {
	fn inner(ty: ResolvedType) -> String {
		match ty {
			ResolvedType::BareType(_, x) => format!(
				"{}{}",
				codegen_ident(x.ident),
				x.generics
					.iter()
					.map(|x| inner(x.clone()))
					.reduce(|acc, b| acc + ", " + &b)
					.map(|x| format!("<{x}>"))
					.unwrap_or_else(String::new)
			),
			ResolvedType::Builtin(_, x) => format!("{x}"),
			ResolvedType::Array(_, lhs, rhs) => format!(
				"Array<{}{}>",
				inner(*lhs),
				rhs.map(|x| ",".to_string() + &codegen_expr(*x))
					.unwrap_or_else(String::new)
			),
			ResolvedType::Ref(_, x, is_mut) => {
				format!("{}<{}>", if is_mut { "Ref" } else { "ConstRef" }, inner(*x))
			}
			ResolvedType::Optional(_, x) => format!("Optional<{}>", inner(*x)),
			ResolvedType::Inferred(_) => "".into(),
		}
	}
	inner(ty)
}

pub fn codegen_ty_ident(ty_ident: ResolvedTypedIdent) -> String {
	format!(
		"{} {}",
		codegen_ty(ty_ident.ty),
		codegen_ident(ty_ident.ident)
	)
}

pub fn codegen_privacy(privacy: Privacy) -> String {
	privacy.to_string()
}

pub fn codegen_stmt(stmt: ResolvedStmt) -> String {
	match stmt {
		ResolvedStmt::Create(_, privacy, ty_ident, _is_mut, expr) => {
			format!(
				"{}{} = {};",
				codegen_privacy(privacy),
				codegen_ty_ident(ty_ident),
				codegen_expr(expr)
			)
		}
		ResolvedStmt::Declare(_, privacy, ty_ident, _is_mut) => {
			format!(
				"{}{};",
				codegen_privacy(privacy),
				codegen_ty_ident(ty_ident)
			)
		}
		ResolvedStmt::Set(_, ident, expr) => {
			format!("{} = {};", codegen_ident(ident), codegen_expr(expr))
		}
		ResolvedStmt::Return(_, expr) => {
			format!("return {};", codegen_expr(expr))
		}
		ResolvedStmt::BareExpr(_, expr) => {
			format!("({});", codegen_expr(expr))
		}
		ResolvedStmt::Unsafe(_, scope) => {
			format!("{{{}}}", codegen_scope(scope))
		}
		ResolvedStmt::Cpp(_, code) => code[..code.len() - 1][1..].to_string(),
		_ => unreachable!(),
	}
}

pub fn codegen_func_noscope(name: &str, func: &ResolvedFunc, semi: bool) -> String {
	let mut code = "".to_string();
	if !func.generics.is_empty() {
		code += "template <";
		for (i, generic) in func.generics.iter().enumerate() {
			code += &format!("typename {}", codegen_mangle(&generic.to_string()));
			if i < func.generics.len() - 1 {
				code += ","
			}
		}
		code += "> ";
	}
	if func.attribs.is_pure {
		code += "constexpr ";
	}
	code += &format!(
		"{} {}(",
		codegen_ty(func.return_ty.clone()),
		codegen_mangle(name)
	);
	for (i, arg) in func.args.iter().enumerate() {
		code += &format!(
			"{} {}",
			codegen_ty(arg.ty.clone()),
			codegen_mangle(&arg.to_string())
		);
		if i < func.args.len() - 1 {
			code += ", ";
		}
	}
	code += ")";
	// TODO: func.attribs.is_mut
	if semi {
		code += ";";
	}
	code
}

pub fn codegen_func_predecl(name: &str, func: &ResolvedFunc) -> String {
	codegen_func_noscope(name, func, true)
}

pub fn codegen_func(name: &str, func: &ResolvedFunc) -> String {
	let body = func.body.clone();
	let mut code = codegen_func_noscope(name, func, false);
	code += "{";
	code += &codegen_scope(body);
	code += "}";
	code
}

pub fn codegen_scope(scope: ResolvedScope) -> String {
	let mut code = "".to_string();
	// TODO: codegen structs
	let mut predecls = "".to_string();
	let mut funcs = "".to_string();
	for (name, func) in scope.data.funcs {
		predecls += &codegen_func_predecl(&name, &func);
		funcs += &codegen_func(&name, &func);
	}
	code += &predecls;
	code += &funcs;
	for stmt in scope.stmts {
		code += &codegen_stmt(stmt);
	}
	code
}

pub fn codegen(scope: ResolvedScope) -> String {
	let mut code = "#include \"chestnut.h\"\nnamespace Chestnut {".to_string();
	code += &codegen_scope(scope);
	code += "}";
	code
}

// TODO: codegen_all_predecls. this would codegen all desired signatures of a
// file or all of them if specified. this would allow for importing, as the
// underlying definitions would be linked together by the compiler itself.
