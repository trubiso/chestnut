#include "lexer.hpp"
#include "parser.hpp"

#include <cerrno>
#include <fstream>
#include <iostream>
#include <string>

struct ParsedFile {
	std::string source;
	AST::Module module;
};

std::optional<ParsedFile> parse_file(std::string_view name) {
	// reading
	std::ifstream file;
	file.open(name.data());
	if (!file.is_open()) {
		std::cerr << "Failed to open source file." << std::endl;
		errno = ENOENT;
		return {};
	}
	std::string source {std::istreambuf_iterator<char>(file), std::istreambuf_iterator<char>()};

	// lexing
	Lexer lexer(source);
	Stream<Token> tokens {std::move(lexer.collect_all())};
	for (auto const& diagnostic : lexer.diagnostics()) { diagnostic.print(lexer.loc(), source); }

	// parsing
	AST::Parser parser {std::move(tokens)};
	AST::Module parsed = parser.parse_all(name);
	for (auto const& diagnostic : parser.diagnostics()) { diagnostic.print(lexer.loc(), source); }

	return ParsedFile{source, std::move(parsed)};
}

int main(void) {
	auto maybe_file = parse_file("source");
	if (!maybe_file.has_value()) return errno;
	ParsedFile parsed_file = std::move(maybe_file.value());
}
