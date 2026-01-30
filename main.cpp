#include "lexer.hpp"
#include "parser.hpp"
#include "resolver.hpp"

#include <cerrno>
#include <fstream>
#include <iostream>
#include <string>

std::optional<std::vector<std::string>> get_sources(std::vector<std::string> const& files_to_parse) {
	std::vector<std::string> sources {};
	for (std::string const& name : files_to_parse) {
		std::ifstream file;
		file.open(name.data());
		if (!file.is_open()) {
			std::cerr << "Failed to open source file '" << name << "'." << std::endl;
			errno = ENOENT;
			return {};
		}
		sources.emplace_back(std::istreambuf_iterator<char>(file), std::istreambuf_iterator<char>());
	}
	return sources;
}

std::tuple<size_t, std::vector<std::tuple<Stream<Token>, FileContext>>>
lex_files(std::vector<std::string> const& sources, std::vector<std::string> const& names) {
	std::vector<std::tuple<Stream<Token>, FileContext>> outputs {};
	assert(sources.size() == names.size());
	size_t diagnostics = 0;
	for (size_t i = 0; i < sources.size(); ++i) {
		FileContext   context {names[i], (FileContext::ID) i, {0}, sources[i]};
		Lexer         lexer(context, sources[i]);
		Stream<Token> tokens {std::move(lexer.collect_all())};
		outputs.push_back({std::move(tokens), lexer.context});
		for (auto const& diagnostic : lexer.diagnostics()) {
			diagnostics++;
			diagnostic.print();
		}
	}
	return {diagnostics, outputs};
}

std::vector<Resolver::ParsedFile> parse_files(std::vector<std::tuple<Stream<Token>, FileContext>>&& lexed_files) {
	std::vector<Resolver::ParsedFile> parsed_files {};
	for (auto&& [tokens, context] : std::move(lexed_files)) {
		AST::Parser parser {context, std::move(tokens)};
		AST::Module parsed = parser.parse_all(context.name);
		for (auto const& diagnostic : parser.diagnostics()) { diagnostic.print(); }

		parsed_files.push_back(
			Resolver::ParsedFile {
				context.file_id,
				std::string(context.source),
				context.name,
				context.loc,
				std::move(parsed),
				{}
			}
		);
	}
	return parsed_files;
}

int main(void) {
	// TODO: at some point, we have to solve folders and how they create submodules

	std::vector<std::string> files_to_parse {"source", "my_module"};

	std::optional<std::vector<std::string>> maybe_sources = get_sources(files_to_parse);
	if (!maybe_sources.has_value()) return errno;
	std::vector<std::string> sources = std::move(maybe_sources.value());

	auto [diagnostic_count, lexed_files] = lex_files(sources, files_to_parse);

	std::vector<Resolver::ParsedFile> parsed_files = parse_files(std::move(lexed_files));

	Resolver resolver {std::move(parsed_files)};
	resolver.resolve();

	for (auto const& file : resolver.parsed_files) { std::cout << file.module << std::endl; }

	for (auto const& file : resolver.parsed_files)
		for (auto const& diagnostic : file.diagnostics) diagnostic.print();

	// resolver.dump();
}
