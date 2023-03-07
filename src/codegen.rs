use crate::resolve::ResolvedScope;

pub fn codegen(scope: ResolvedScope) -> String {
	let mut code = "".to_string();
	for (_, func) in scope.funcs {
		code += &format!("[[nodiscard]] {} {}(", func.return_ty, func.name);
		for (i, arg) in func.args.iter().enumerate() {
			code += &format!("{} {}", arg.ty, arg.name);
			if i < func.args.len() - 1 {
				code += ", ";
			}
		}
		code += ") {";
		// TODO: store resolved stmts in resolver :D
		code += "}";
	}
	code
}
