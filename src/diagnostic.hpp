#pragma once
#include "out_fmt.hpp"
#include "span.hpp"

#include <cstdint>
#include <initializer_list>
#include <optional>
#include <string>
#include <vector>

struct FileContext {
	typedef uint32_t ID;

	static constexpr ID BUILT_IN_ID = UINT32_MAX;

	std::string name;

	ID file_id;
	/// An array containing the start of each of the lines of code in a file.
	std::vector<size_t> loc;
	// we don't want to own the source!
	std::string_view source;
};

struct Diagnostic {
	struct Sample {
		struct Label {
			Span                       span;
			std::optional<std::string> label;
			OutFmt::Color              color;

			explicit Label(Span span, std::string&& label, OutFmt::Color color)
				: span {span}
				, label {label}
				, color {color} {}

			explicit Label(Span span, OutFmt::Color color) : span {span}, label {}, color {color} {}
		};

		explicit Sample(FileContext const& context, std::initializer_list<Label> labels)
			: title {}
			, labels {labels}
			, context {context} {}

		explicit Sample(FileContext const& context, std::string&& title, std::initializer_list<Label> labels)
			: title {title}
			, labels {labels}
			, context {context} {}

		explicit Sample(FileContext const& context, Span span, OutFmt::Color color)
			: Sample {context, {Label {span, color}}} {}

		explicit Sample(FileContext const& context, Span span) : Sample {context, span, OutFmt::Color::Red} {}

		/// All titles will be prefixed with "note:".
		std::optional<std::string> title;
		/// There must be at least one label!!!
		std::vector<Label> labels;

		FileContext context;

		Span span() const;
		void print() const;
	};

	enum class Severity { Error, Warning } severity;
	std::string                title;
	std::optional<std::string> subtitle;
	std::vector<Sample>        samples;

	explicit Diagnostic(
		Severity                      severity,
		std::string&&                 title,
		std::optional<std::string>    subtitle = {},
		std::initializer_list<Sample> samples  = {}
	)
		: severity {severity}
		, title {title}
		, subtitle {subtitle}
		, samples {samples} {}

	explicit Diagnostic(
		Severity                   severity,
		std::string&&              title,
		std::optional<std::string> subtitle = {},
		std::vector<Sample>&&      samples  = {}
	)
		: severity {severity}
		, title {title}
		, subtitle {subtitle}
		, samples {std::move(samples)} {}

	static inline Diagnostic
	error(std::string&&                 title,
	      std::optional<std::string>    subtitle = {},
	      std::initializer_list<Sample> samples  = {}) {
		return Diagnostic(Severity::Error, std::move(title), subtitle, samples);
	}

	static inline Diagnostic
	error(std::string&& title, std::optional<std::string> subtitle, std::vector<Sample>&& samples) {
		return Diagnostic(Severity::Error, std::move(title), subtitle, std::move(samples));
	}

	static inline Diagnostic error(std::string&& title, std::initializer_list<Sample> samples) {
		return Diagnostic(Severity::Error, std::move(title), {}, samples);
	}

	static inline Diagnostic
	warning(std::string&&                 title,
	        std::optional<std::string>    subtitle = {},
	        std::initializer_list<Sample> samples  = {}) {
		return Diagnostic(Severity::Warning, std::move(title), subtitle, samples);
	}

	static inline Diagnostic warning(std::string&& title, std::initializer_list<Sample> samples) {
		return Diagnostic(Severity::Warning, std::move(title), {}, samples);
	}

	void print() const;
};
