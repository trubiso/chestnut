#include "test.hpp"

#include "lexer.hpp"
#include "parser.hpp"
#include "resolver.hpp"

#include <cstring>
#include <regex>
#include <sstream>

std::optional<std::string> process_subtitle(std::optional<std::string> const& subtitle) {
	if (!subtitle.has_value()) return {};
	std::string without_newlines = std::regex_replace(subtitle.value(), std::regex("\n"), " | ");
	std::string without_tabs     = std::regex_replace(without_newlines, std::regex("\t"), "");
	return without_tabs;
}

std::optional<std::string> compare_diagnostics_inner(
	std::vector<Diagnostic> const&               diagnostics,
	std::vector<Test::ExpectedDiagnostic> const& expected_diagnostics
) {
	if (diagnostics.size() != expected_diagnostics.size()) return "\tincorrect amount of diagnostics";
	for (size_t i = 0; i < diagnostics.size(); ++i) {
		Diagnostic const&               diagnostic          = diagnostics.at(i);
		Test::ExpectedDiagnostic const& expected_diagnostic = expected_diagnostics.at(i);
		if (diagnostic.severity != expected_diagnostic.severity)
			return "\tdifferent severity for one of the diagnostics";
		if (diagnostic.title != expected_diagnostic.title)
			return "\tdifferent title for one of the diagnostics";
		if (diagnostic.subtitle.has_value() != expected_diagnostic.subtitle.has_value())
			return "\tdifferent subtitle for one of the diagnostics";
		if (diagnostic.subtitle.has_value()
		    && process_subtitle(diagnostic.subtitle) != expected_diagnostic.subtitle)
			return "\tdifferent subtitle for one of the diagnostics";
	}
	return std::nullopt;
}

std::optional<std::string> compare_diagnostics(
	std::vector<Diagnostic> const&               diagnostics,
	std::vector<Test::ExpectedDiagnostic> const& expected_diagnostics
) {
	return compare_diagnostics_inner(diagnostics, expected_diagnostics).transform([&](auto&& value) {
		std::stringstream expected_diagnostics_str {};
		if (expected_diagnostics.empty()) expected_diagnostics_str << "\n\t\t\t(no diagnostics)";
		else
			for (auto const& diagnostic : expected_diagnostics) {
				expected_diagnostics_str << "\n\t\t\t* severity: ";
				switch (diagnostic.severity) {
				case Diagnostic::Severity::Error:   expected_diagnostics_str << "Error"; break;
				case Diagnostic::Severity::Warning: expected_diagnostics_str << "Warning"; break;
				}
				expected_diagnostics_str << ", title: \"" << diagnostic.title << '"';
				if (diagnostic.subtitle.has_value()) {
					expected_diagnostics_str
						<< ", subtitle: \""
						<< diagnostic.subtitle.value()
						<< '"';
				}
			}

		std::stringstream diagnostics_str {};
		if (diagnostics.empty()) diagnostics_str << "\n\t\t\t(no diagnostics)";
		else
			for (auto const& diagnostic : diagnostics) {
				diagnostics_str << "\n\t\t\t* severity: ";
				switch (diagnostic.severity) {
				case Diagnostic::Severity::Error:   diagnostics_str << "Error"; break;
				case Diagnostic::Severity::Warning: diagnostics_str << "Warning"; break;
				}
				diagnostics_str << ", title: \"" << diagnostic.title << '"';
				if (diagnostic.subtitle.has_value()) {
					diagnostics_str
						<< ", subtitle: \""
						<< process_subtitle(diagnostic.subtitle).value()
						<< '"';
				}
			}

		return std::format(
			"{}\n\t\texpected diagnostics:{}\n\t\tactual diagnostics:{}",
			std::move(value),
			expected_diagnostics_str.str(),
			diagnostics_str.str()
		);
	});
}

std::optional<std::string> compare_outputs(std::string const& output, std::string const& expected_output) {
	if (output == expected_output) return std::nullopt;
	return std::format("\twrong output\n\t\texpected output:\n{}\n\t\tactual output:\n{}", expected_output, output);
}

std::optional<std::string> Test::run() const {
	FileContext             context {"test", 0, {0}, source};
	std::vector<Diagnostic> diagnostics {};

	// lexer
	Lexer         lexer {context, source};
	Stream<Token> tokens {std::move(lexer.collect_all())};
	std::move(lexer.diagnostics().begin(), lexer.diagnostics().end(), std::back_inserter(diagnostics));
	if (pass == Pass::Lexer) {
		if (std::holds_alternative<std::vector<ExpectedDiagnostic>>(expected_output)) {
			return compare_diagnostics(
				diagnostics,
				std::get<std::vector<ExpectedDiagnostic>>(expected_output)
			);
		} else {
			std::stringstream output {};
			for (size_t i = 0; i < tokens.size(); ++i) { output << tokens.at(i).value(); }
			return compare_outputs(output.str(), std::get<ExpectedOutput>(expected_output).output);
		}
	}

	// parser
	AST::Parser             parser {context, std::move(tokens)};
	AST::Module             parsed             = parser.parse_all(context.name);
	std::vector<Diagnostic> parser_diagnostics = parser.diagnostics();
	std::move(parser_diagnostics.begin(), parser_diagnostics.end(), std::back_inserter(diagnostics));
	if (pass == Pass::Parser) {
		if (std::holds_alternative<std::vector<ExpectedDiagnostic>>(expected_output)) {
			return compare_diagnostics(
				diagnostics,
				std::get<std::vector<ExpectedDiagnostic>>(expected_output)
			);
		} else {
			std::stringstream output {};
			output << parsed;
			return compare_outputs(output.str(), std::get<ExpectedOutput>(expected_output).output);
		}
	}

	// we have to push after creating because otherwise C++ tries copying everything, as per usual
	std::vector<Resolver::ParsedFile> parsed_files {};
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

	// resolver
	// FIXME: we can't really test cross-file imports
	Resolver                resolver {std::move(parsed_files)};
	std::vector<IR::Module> modules = resolver.resolve();
	if (modules.size() != 1) return "incorrect module count (somehow)";
	if (pass == Pass::Resolver) {
		if (std::holds_alternative<std::vector<ExpectedDiagnostic>>(expected_output)) {
			return compare_diagnostics(
				diagnostics,
				std::get<std::vector<ExpectedDiagnostic>>(expected_output)
			);
		} else {
			std::stringstream output {};
			resolver.print(output, modules.at(0));
			return compare_outputs(output.str(), std::get<ExpectedOutput>(expected_output).output);
		}
	}

	assert(false && "there is literally no other test kind, how did you get here?");

	return std::nullopt;
}

enum class ExpectedOutputKind { Diagnostics, Output };

std::tuple<Test::Pass, ExpectedOutputKind> read_first_line(std::string_view line_sv) {
	// first line syntax: "// pass: " Pass ", expect: " ExpectedOutputKind
	assert(line_sv.starts_with("// pass: ") && "file must begin with '// pass: '");
	std::string_view pass_kind = line_sv.substr(strlen("// pass: "));
	std::string_view expect_bit;

	// pass kind
	Test::Pass pass;
	if (pass_kind.starts_with("Lexer")) {
		pass       = Test::Pass::Lexer;
		expect_bit = pass_kind.substr(strlen("Lexer"));
	} else if (pass_kind.starts_with("Parser")) {
		pass       = Test::Pass::Parser;
		expect_bit = pass_kind.substr(strlen("Parser"));
	} else if (pass_kind.starts_with("Resolver")) {
		pass       = Test::Pass::Resolver;
		expect_bit = pass_kind.substr(strlen("Resolver"));
	} else assert(false && "pass must be 'Lexer', 'Parser' or 'Resolver'");

	// expectation
	assert(expect_bit.starts_with(", expect: "));
	std::string_view   expectation = expect_bit.substr(strlen(", expect: "));
	ExpectedOutputKind expected_output_kind;
	if (expectation == "Diagnostics") expected_output_kind = ExpectedOutputKind::Diagnostics;
	else if (expectation == "Output") expected_output_kind = ExpectedOutputKind::Output;
	else assert(false && "expectation must be 'Diagnostics' or 'Output'");

	return {pass, expected_output_kind};
}

Test::ExpectedDiagnostic parse_expected_diagnostic(std::string_view line_sv) {
	// diagnostic syntax: "// severity: " Severity ", title: " <quoted title> [", subtitle: " <quoted subtitle>]
	assert(line_sv.starts_with("// severity: ") && "expected diagnostic must begin with '// severity: '");
	std::string_view severity_kind = line_sv.substr(strlen("// severity: "));
	std::string_view title_bit;

	// severity
	Diagnostic::Severity severity;
	if (severity_kind.starts_with("Error")) {
		severity  = Diagnostic::Severity::Error;
		title_bit = severity_kind.substr(strlen("Error"));
	} else if (severity_kind.starts_with("Warning")) {
		severity  = Diagnostic::Severity::Warning;
		title_bit = severity_kind.substr(strlen("Warning"));
	} else assert(false && "expected diagnostic severity must be 'Error' or 'Warning'");

	// title
	assert(title_bit.starts_with(", title: \"")
	       && "title must be introduced by ', title: \"' (remember the quotes!)");
	std::string_view title_substr = title_bit.substr(strlen(", title: \""));
	size_t           title_size   = 0;
	for (size_t i = 0; i < title_substr.size(); ++i)
		if (title_substr.at(i) == '"') {
			title_size = i;
			break;
		}
	assert(title_size != 0 && "title must not be empty");
	std::string title {title_substr.substr(0, title_size)};
	if (!title_bit.contains("\", subtitle: \"")) return Test::ExpectedDiagnostic {severity, title, std::nullopt};
	std::string_view subtitle_bit = title_substr.substr(title_size + 1);
	if (!subtitle_bit.starts_with(", subtitle: \""))
		return Test::ExpectedDiagnostic {severity, title, std::nullopt};

	// subtitle
	std::string_view subtitle_substr = subtitle_bit.substr(strlen(", subtitle: \""));
	size_t           subtitle_size   = 0;
	for (size_t i = 0; i < subtitle_substr.size(); ++i)
		if (subtitle_substr.at(i) == '"') {
			subtitle_size = i;
			break;
		}
	assert(subtitle_size != 0 && "subtitle must not be empty");
	std::string subtitle {subtitle_substr.substr(0, subtitle_size)};
	return Test::ExpectedDiagnostic {severity, title, subtitle};
}

Test Test::from_file(std::string&& source) {
	std::istringstream stream(source);
	std::string        line;
	bool               got_line = bool(std::getline(stream, line));
	// we need at least one line per test
	assert(got_line && "test must have at least one line of code");
	auto [pass, expected_output_kind] = read_first_line(line);

	// now the following lines are either diagnostics or output
	if (expected_output_kind == ExpectedOutputKind::Diagnostics) {
		std::vector<Test::ExpectedDiagnostic> expected_diagnostics {};
		while (std::getline(stream, line) && line.starts_with("// severity: ")) {
			expected_diagnostics.push_back(parse_expected_diagnostic(line));
		}
		return Test {expected_diagnostics, std::move(source), pass};
	} else {
		// for output, it starts with a line of just /*, then the output, then a line of just */
		got_line = bool(std::getline(stream, line));
		assert(got_line
		       && "expected output test must consist of the header followed by block-commented output");
		assert(line == "/*" && "expected output test must have block-commented output");
		std::string expected_output = "";
		while ((got_line = bool(std::getline(stream, line))) && line != "*/") {
			expected_output += "\n" + line;
		}
		if (!got_line || line != "*/")
			assert(false && "expected output test did not close block-commented output");
		// trim out the first newline
		expected_output = expected_output.substr(1);
		return Test {ExpectedOutput {std::move(expected_output)}, std::move(source), pass};
	}
}
