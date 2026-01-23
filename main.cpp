#include "lexer.hpp"
#include "parser.hpp"

#include <cerrno>
#include <fstream>
#include <iostream>
#include <string>

int main(void) {
	std::ifstream file;
	file.open("source");
	if (!file.is_open()) {
		std::cerr << "Failed to open source file." << std::endl;
		return ENOENT;
	}
	std::string source {std::istreambuf_iterator<char>(file), std::istreambuf_iterator<char>()};

	Lexer lexer(source);

	Diagnostic diag(Diagnostic::Severity::Warning, "died", "mingle", {Diagnostic::Sample({Diagnostic::Sample::Label(Span(15, 43), "not ok", OutFmt::Color::Red)})});

	Stream<Token> tokens {std::move(lexer.collect_all())};

	for (auto const& diagnostic : lexer.diagnostics()) { diagnostic.print(lexer.loc(), source); }

	AST::Parser parser {std::move(tokens)};

	// TODO: remove this once the compiler knows when to stop
	size_t iters = 1000;
	while (iters-- && parser.advance());
	
	for (auto const& diagnostic : parser.diagnostics()) { diagnostic.print(lexer.loc(), source); }

	std::cout << std::endl;
}
