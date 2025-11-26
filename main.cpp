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

	while (lexer.advance());

	for (auto const& token : lexer.tokens()) {
		std::cout << token << std::endl;
	}

	std::cout << lexer.tokens().size() << std::endl;
}
