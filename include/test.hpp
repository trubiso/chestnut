#pragma once
#include "diagnostic.hpp"

#include <optional>
#include <string>
#include <variant>
#include <vector>

struct Test {
	/// Expect a diagnostic with the following title and subtitle.
	struct ExpectedDiagnostic {
		Diagnostic::Severity       severity;
		std::string                title;
		std::optional<std::string> subtitle;
	};

	/// Expect a (pretty-printed) output and no diagnostics.
	struct ExpectedOutput {
		std::string output;
	};

	/// What the result from running the test is expected to be.
	std::variant<std::vector<ExpectedDiagnostic>, ExpectedOutput> expected_output;
	/// The code for the test.
	std::string source;
	/// Up to which compiler pass the test should run.
	enum class Pass { Lexer, Parser, Resolver } pass;
	/// Run the test, returning nothing if it passed and a string buffer containing an error if it failed.
	std::optional<std::string> run() const;
	/// Create a test from a source file, parsing the expected output and pass from the source.
	static Test from_file(std::string&& source);
};
