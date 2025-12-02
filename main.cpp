#include "lexer.hpp"

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

	Diagnostic diag(Diagnostic::Severity::Note, "died", "mingle", {Diagnostic::Label(Span(15, 43), "not ok")});

	while (lexer.advance());

	for (auto const& token : lexer.tokens()) {
		std::cout << token << std::endl;
	}

	for (auto const& diagnostic : lexer.diagnostics()) {
		diagnostic.print(lexer.loc(), source);
	}
	std::cout << lexer.tokens().size() << std::endl;
	for (auto const loc : lexer.loc()) std::cout << loc << " ";
	std::cout << std::endl;
}
