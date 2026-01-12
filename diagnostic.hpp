#include "out_fmt.hpp"
#include "span.hpp"

#include <initializer_list>
#include <optional>
#include <string>
#include <vector>

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

		/// All titles will be prefixed with "note:".
		std::optional<std::string> title;
		/// There must be at least one label!!!
		std::vector<Label> labels;

		explicit Sample(std::initializer_list<Label> labels) : title {}, labels {labels} {}

		explicit Sample(std::string&& title, std::initializer_list<Label> labels)
			: title {title}
			, labels {labels} {}

		explicit Sample(Span span, OutFmt::Color color)
			: Sample {
				  Label {span, color}
                } {}

		explicit Sample(Span span) : Sample {span, OutFmt::Color::Red} {}

		Span span() const;
		void print(std::vector<size_t> const& loc, std::string_view code) const;
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

	static inline Diagnostic
	error(std::string&&                 title,
	      std::optional<std::string>    subtitle = {},
	      std::initializer_list<Sample> samples  = {}) {
		return Diagnostic(Severity::Error, std::move(title), subtitle, samples);
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

	void print(std::vector<size_t> const& loc, std::string_view code) const;
};
