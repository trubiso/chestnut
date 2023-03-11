use crate::{
	lexer::{NumberLiteralKind, NumberLiteralRepr},
	parser::types::Privacy,
	resolve::{
		ResolvedExpr, ResolvedFunc, ResolvedScope, ResolvedStmt, ResolvedType, ResolvedTypedIdent,
	},
};

pub fn comma<T>(args: Vec<T>, closure: fn(&T) -> String) -> String {
	args.iter()
		.map(closure)
		.reduce(|acc, b| acc + "," + &b)
		.unwrap_or_else(String::new)
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
		ResolvedExpr::Identifier(_, ident) => ident.to_string(),
		ResolvedExpr::BinaryOp(_, lhs, op, rhs) => {
			format!("({}{op}{})", codegen_expr(*lhs), codegen_expr(*rhs))
		}
		ResolvedExpr::UnaryOp(_, op, val) => format!("({op}{})", codegen_expr(*val)),
		ResolvedExpr::Lambda(_, func) => {
			format!(
				"([&]({}){{{}}})",
				comma(func.args, |x| codegen_ty_ident(x.clone().as_ty_ident())),
				codegen_scope(func.body),
			)
		}
		ResolvedExpr::Call(_, callee, args) => format!(
			"{}({})",
			codegen_expr(*callee),
			comma(args, |x| codegen_expr(x.clone()))
		),
	}
}

pub fn codegen_ty(ty: ResolvedType) -> String {
	fn inner(ty: ResolvedType, comes_from_mut: bool) -> String {
		let m = if comes_from_mut { "" } else { " const" };
		match ty {
			ResolvedType::BareType(_, x) => format!(
				"{}{}{m}",
				x.ident.to_string(),
				x.generics
					.iter()
					.map(|x| inner(x.clone(), false))
					.reduce(|acc, b| acc + ", " + &b)
					.map(|x| format!("<{x}>"))
					.unwrap_or_else(String::new)
			),
			ResolvedType::Builtin(_, x) => format!("{x}{m}"),
			ResolvedType::Array(_, lhs, rhs) => format!(
				"Array<{}{}>{m}",
				inner(*lhs, false),
				rhs.map(|x| ",".to_string() + &codegen_expr(*x))
					.unwrap_or_else(String::new)
			),
			ResolvedType::Ref(_, x) => format!("Ref<{}>{m}", inner(*x, false)),
			ResolvedType::Optional(_, x) => format!("Optional<{}>{m}", inner(*x, false)),
			ResolvedType::Mut(_, x) => inner(*x, true),
			ResolvedType::Inferred(_) => "".into(),
		}
	}
	inner(ty, false)
}

pub fn codegen_ty_ident(ty_ident: ResolvedTypedIdent) -> String {
	format!("{} {}", codegen_ty(ty_ident.ty), ty_ident.ident.to_string())
}

pub fn codegen_privacy(privacy: Privacy) -> String {
	privacy.to_string()
}

pub fn codegen_stmt(stmt: ResolvedStmt) -> String {
	match stmt {
		ResolvedStmt::Create(_, privacy, ty_ident, expr) => {
			format!(
				"{}{} = {};",
				codegen_privacy(privacy),
				codegen_ty_ident(ty_ident),
				codegen_expr(expr)
			)
		}
		ResolvedStmt::Declare(_, privacy, ty_ident) => {
			format!(
				"{}{};",
				codegen_privacy(privacy),
				codegen_ty_ident(ty_ident)
			)
		}
		ResolvedStmt::Set(_, ident, expr) => {
			format!("{} = {};", ident.to_string(), codegen_expr(expr))
		}
		ResolvedStmt::Return(_, expr) => {
			format!("return {};", codegen_expr(expr))
		}
		ResolvedStmt::BareExpr(_, expr) => {
			format!("({});", expr)
		}
		ResolvedStmt::Unsafe(_, scope) => {
			format!("{{{}}}", codegen_scope(scope))
		}
	}
}

pub fn codegen_func_noscope(func: &ResolvedFunc, semi: bool) -> String {
	let mut code = "".to_string();
	if !func.generics.is_empty() {
		code += "template <";
		for (i, generic) in func.generics.iter().enumerate() {
			code += &format!("typename {generic}");
			if i < func.generics.len() - 1 {
				code += ","
			}
		}
		code += "> ";
	}
	if func.attribs.is_pure {
		code += "constexpr ";
	}
	code += &format!("{} {}(", codegen_ty(func.return_ty.clone()), func.name);
	for (i, arg) in func.args.iter().enumerate() {
		code += &format!("{} {}", codegen_ty(arg.ty.clone()), arg.name);
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

pub fn codegen_func_predecl(func: &ResolvedFunc) -> String {
	codegen_func_noscope(func, true)
}

pub fn codegen_func(func: &ResolvedFunc) -> String {
	let body = func.body.clone();
	let mut code = codegen_func_noscope(func, false);
	code += "{";
	code += &codegen_scope(body);
	code += "}";
	code
}

pub fn codegen_scope(scope: ResolvedScope) -> String {
	let mut code = "".to_string();
	let mut predecls = "".to_string();
	let mut funcs = "".to_string();
	for (_, func) in scope.data.funcs {
		predecls += &codegen_func_predecl(&func);
		funcs += &codegen_func(&func);
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
