#include "lexer.hpp"
#include "parser.hpp"
#include "resolver.hpp"

#include <cerrno>
#include <fstream>
#include <iostream>
#include <string>

std::optional<Resolver::ParsedFile> parse_file(std::string_view name) {
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
	Lexer         lexer(source);
	Stream<Token> tokens {std::move(lexer.collect_all())};
	for (auto const& diagnostic : lexer.diagnostics()) { diagnostic.print(lexer.loc(), source); }

	// parsing
	AST::Parser parser {std::move(tokens)};
	AST::Module parsed = parser.parse_all(name);
	for (auto const& diagnostic : parser.diagnostics()) { diagnostic.print(lexer.loc(), source); }

	return Resolver::ParsedFile {source, lexer.loc(), std::move(parsed)};
}

int main(void) {
	// TODO: at some point, we have to solve folders and how they create submodules
	std::vector<Resolver::ParsedFile> parsed_files {};
	std::vector<std::string>          files_to_parse {"source", "my_module"};
	for (auto const& file : files_to_parse) {
		auto maybe_file = parse_file(file);
		if (!maybe_file.has_value()) return errno;
		parsed_files.push_back(std::move(maybe_file.value()));
	}

	Resolver resolver {std::move(parsed_files)};
	resolver.resolve();

	for (auto const& file : resolver.parsed_files) { std::cout << file.module.name.value << std::endl; }

	for (auto const& diagnostic : resolver.diagnostics()) {
		diagnostic.print(resolver.parsed_files[0].loc, resolver.parsed_files[0].source);
	}
}
