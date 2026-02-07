#include "codegen.hpp"
#include "lexer.hpp"
#include "parser.hpp"
#include "resolver.hpp"
#include "test.hpp"

#include <cerrno>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <string>

size_t errors = 0, warnings = 0;

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

std::vector<std::tuple<Stream<Token>, FileContext>>
lex_files(std::vector<std::string> const& sources, std::vector<std::string> const& names) {
	std::vector<std::tuple<Stream<Token>, FileContext>> outputs {};
	assert(sources.size() == names.size());
	for (size_t i = 0; i < sources.size(); ++i) {
		FileContext   context {names[i], (FileContext::ID) i, {0}, sources[i]};
		Lexer         lexer(context, sources[i]);
		Stream<Token> tokens {std::move(lexer.collect_all())};
		outputs.push_back({std::move(tokens), lexer.context});
		for (auto const& diagnostic : lexer.diagnostics()) {
			diagnostic.print();
			if (diagnostic.severity == Diagnostic::Severity::Error) errors++;
			else warnings++;
		}
	}
	return outputs;
}

std::vector<Resolver::ParsedFile> parse_files(std::vector<std::tuple<Stream<Token>, FileContext>>&& lexed_files) {
	std::vector<Resolver::ParsedFile> parsed_files {};
	for (auto&& [tokens, context] : std::move(lexed_files)) {
		AST::Parser parser {context, std::move(tokens)};
		AST::Module parsed = parser.parse_all(context.name);
		for (auto const& diagnostic : parser.diagnostics()) {
			diagnostic.print();

			if (diagnostic.severity == Diagnostic::Severity::Error) errors++;
			else warnings++;
		}

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

bool run_tests() {
	std::string              tests_folder = "tests";
	std::vector<std::string> sources {};
	std::vector<std::string> paths {};
	for (std::filesystem::directory_entry const& entry :
	     std::filesystem::recursive_directory_iterator(tests_folder)) {
		if (!entry.is_regular_file()) continue;

		std::ifstream file(entry.path());
		if (!file.is_open()) {
			std::cerr << "Failed to open test file '" << entry.path().string() << "'." << std::endl;
			errno = ENOENT;
			return false;
		}
		sources.emplace_back(std::istreambuf_iterator<char>(file), std::istreambuf_iterator<char>());
		paths.push_back(entry.path().string().substr(tests_folder.size() + 1));
	}
	assert(sources.size() == paths.size());
	bool all_passed = true;
	for (size_t i = 0; i < sources.size(); ++i) {
		Test                       test  = Test::from_file(std::move(sources.at(i)));
		std::optional<std::string> error = test.run();
		if (error.has_value()) {
			all_passed = false;
			OutFmt::bg(OutFmt::Color::Red);
			OutFmt::fg(OutFmt::Color::BrightWhite);
			OutFmt::set_bold();
			std::cout << "[ F ]";
			OutFmt::reset();
			std::cout << " " << paths.at(i) << '\n' << error.value() << std::endl;
		} else {
			OutFmt::bg(OutFmt::Color::BrightGreen);
			OutFmt::fg(OutFmt::Color::Black);
			OutFmt::set_bold();
			std::cout << "[ P ]";
			OutFmt::reset();
			std::cout << " " << paths.at(i) << std::endl;
		}
	}
	return all_passed;
}

int main(void) {
	run_tests();
	return 0;

	// TODO: at some point, we have to solve folders and how they create submodules
	std::vector<std::string> files_to_parse {"my_module"};

	std::optional<std::vector<std::string>> maybe_sources = get_sources(files_to_parse);
	if (!maybe_sources.has_value()) return errno;
	std::vector<std::string> sources = std::move(maybe_sources.value());

	auto lexed_files = lex_files(sources, files_to_parse);

	std::vector<Resolver::ParsedFile> parsed_files = parse_files(std::move(lexed_files));

	Resolver                resolver {std::move(parsed_files)};
	std::vector<IR::Module> modules = resolver.resolve();

	for (auto const& file : resolver.parsed_files)
		for (auto const& diagnostic : file.diagnostics) {
			diagnostic.print();
			if (diagnostic.severity == Diagnostic::Severity::Error) errors++;
			else warnings++;
		}

	// if there are errors, do not proceed to codegen
	if (errors > 0) {
		std::cerr << "aborting due to " << errors << " error";
		if (errors != 1) std::cerr << 's';
		if (warnings > 0) {
			std::cerr << " and " << warnings << " warning";
			if (warnings != 1) std::cerr << 's';
		}
		std::cerr << std::endl;
		return 1;
	}

	// if there aren't any errors but there are warnings, tell the user the count
	if (warnings > 0) {
		std::cerr << "produced " << warnings << " warning";
		if (warnings != 1) std::cerr << 's';
		std::cerr << std::endl;
	}

	CodeGenerator generator(resolver.export_symbols());
	generator.process(modules);
}
