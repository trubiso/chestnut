#include "analyzer.hpp"
#include "codegen.hpp"
#include "lexer.hpp"
#include "parser.hpp"
#include "resolver.hpp"
#include "test.hpp"

#include <boost/program_options.hpp>
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

std::vector<Resolver::ParsedFile>
parse_files(std::vector<std::tuple<Stream<Token>, FileContext>>&& lexed_files, bool print_ast) {
	std::vector<Resolver::ParsedFile> parsed_files {};
	for (auto&& [tokens, context] : std::move(lexed_files)) {
		AST::Parser parser {context, std::move(tokens)};
		AST::Module parsed = parser.parse_all(context.name);
		if (print_ast) { std::cout << "=== " << context.name << " ===\n" << parsed << std::endl; }
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
	size_t tests_passed = 0;
	for (size_t i = 0; i < sources.size(); ++i) {
		Test                       test  = Test::from_file(std::move(sources.at(i)));
		std::optional<std::string> error = test.run();
		if (error.has_value()) {
			OutFmt::bg(OutFmt::Color::Red);
			OutFmt::fg(OutFmt::Color::BrightWhite);
			OutFmt::set_bold();
			std::cout << "[ F ]";
			OutFmt::reset();
			std::cout << " " << paths.at(i) << '\n' << error.value() << '\n';
		} else {
			tests_passed++;
			OutFmt::bg(OutFmt::Color::BrightGreen);
			OutFmt::fg(OutFmt::Color::Black);
			OutFmt::set_bold();
			std::cout << "[ P ]";
			OutFmt::reset();
			std::cout << " " << paths.at(i) << '\n';
		}
	}
	std::cout << tests_passed << " test" << (tests_passed == 1 ? "" : "s") << " passed";
	if (tests_passed < sources.size()) std::cout << ", " << sources.size() - tests_passed << " failed";
	std::cout << std::endl;
	return tests_passed == sources.size();
}

static std::string compiler_name = "chc";

struct Input {
	bool                        run_compiler_tests;
	bool                        print_ast;
	bool                        print_ir;
	CodeGenerator::Optimization optimization = CodeGenerator::Optimization::O0;
	std::vector<std::string>    inputs;
	std::string                 output;
};

Input get_input(int argc, char** argv) {
	Input input;
	try {
		boost::program_options::options_description options("options");
		// TODO: add option for optimization level
		options.add_options()("help,h", "show help message")("run-compiler-tests", "run compiler-internal tests")(
			"output,o",
			boost::program_options::value<std::string>(&input.output)->default_value("output.o"),
			"output file"
		)("print-ast", "print AST and quit")("print-ir", "print IR before codegen");

		boost::program_options::options_description hidden("");
		hidden.add_options()("inputs", boost::program_options::value<std::vector<std::string>>());

		boost::program_options::options_description visible;
		visible.add(options);

		boost::program_options::options_description all;
		all.add(visible).add(hidden);

		boost::program_options::positional_options_description positional;
		positional.add("inputs", -1);

		boost::program_options::variables_map variables_map;
		boost::program_options::store(
			boost::program_options::command_line_parser(argc, argv)
				.options(all)
				.positional(positional)
				.run(),
			variables_map
		);
		boost::program_options::notify(variables_map);

		if (variables_map.count("help")) {
			std::cout << compiler_name << " chestnut compiler\n\n";
			std::cout << "usage: " << compiler_name << " [options] file...\n";
			std::cout << visible;
			std::exit(0);
		}

		input.run_compiler_tests = variables_map.count("run-compiler-tests");
		input.print_ast          = variables_map.count("print-ast");
		input.print_ir           = variables_map.count("print-ir");
		if (input.run_compiler_tests) return input;

		if (!variables_map.count("inputs")) {
			std::cerr << "error: no input files" << std::endl;
			std::exit(1);
		}

		input.inputs = std::move(variables_map.at("inputs").as<std::vector<std::string>>());
		return input;
	} catch (boost::program_options::error const& error) {
		std::cerr << "argument error: " << error.what() << std::endl;
		std::exit(1);
	}
}

int main(int argc, char** argv) {
	Input input = get_input(argc, argv);

	if (input.run_compiler_tests) {
		bool passed = run_tests();
		return !passed;
	}

	// TODO: at some point, we have to solve folders and how they create submodules
	for (auto const& filename : input.inputs) {
		if (filename.contains('/') || filename.contains('\\')) {
			std::cerr << "todo: files within folders are unsupported" << std::endl;
			std::exit(1);
		}
	}

	std::optional<std::vector<std::string>> maybe_sources = get_sources(input.inputs);
	if (!maybe_sources.has_value()) return errno;
	std::vector<std::string> sources = std::move(maybe_sources.value());

	auto lexed_files = lex_files(sources, input.inputs);

	std::vector<Resolver::ParsedFile> parsed_files = parse_files(std::move(lexed_files), input.print_ast);

	if (input.print_ast) std::exit(0);

	Resolver                resolver {std::move(parsed_files)};
	std::vector<IR::Module> modules = resolver.resolve(input.print_ir);

	std::vector<Analyzer::ResolvedFile> resolved_files {};
	resolved_files.reserve(parsed_files.size());
	for (size_t i = 0; i < resolver.parsed_files.size(); ++i) {
		Resolver::ParsedFile parsed_file = std::move(resolver.parsed_files[i]);
		resolved_files.push_back(
			Analyzer::ResolvedFile {
				parsed_file.file_id,
				std::move(parsed_file.source),
				std::move(parsed_file.name),
				std::move(parsed_file.loc),
				std::move(modules[i]),
				std::move(parsed_file.diagnostics)
			}
		);
	}

	Analyzer analyzer(resolver.export_symbols(), std::move(resolved_files));
	if (input.print_ir) std::cout << "=== POST-ANALYSIS ===" << std::endl;
	analyzer.analyze(input.print_ir);
	for (auto const& file : analyzer.resolved_files)
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

	CodeGenerator           generator(std::move(analyzer.symbols));
	std::vector<IR::Module> analyzed_modules {};
	analyzed_modules.reserve(analyzer.resolved_files.size());
	for (Analyzer::ResolvedFile& file : analyzer.resolved_files) analyzed_modules.push_back(std::move(file.module));
	generator.process(analyzed_modules, input.output, input.optimization);
}
