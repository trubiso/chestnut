#include "span.hpp"

#include <cstddef>
#include <initializer_list>
#include <optional>
#include <string>
#include <string_view>
#include <vector>

struct Diagnostic {
public:
	enum class Severity { Error, Warning, Note };

	struct Label {
		Span                       span;
		std::optional<std::string> label;
		std::optional<Severity>    color_override = {};

		explicit Label(
			Span                       span,
			std::optional<std::string> label          = {},
			std::optional<Severity>    color_override = {}
		)
			: span(span)
			, label(label)
			, color_override(color_override) {}
	};

	Severity                   severity;
	std::string                title;
	std::optional<std::string> subtitle = {};
	std::vector<Label>         labels;

	explicit Diagnostic(
		Severity                     severity,
		std::string                  title,
		std::optional<std::string>   subtitle = {},
		std::initializer_list<Label> labels   = {}
	)
		: severity(severity)
		, title(title)
		, subtitle(subtitle)
		, labels(labels) {}

	inline Diagnostic& add_label(Label&& label) {
		labels.push_back(label);
		return *this;
	}

	void print(std::vector<size_t> const& loc, std::string_view code) const;
};
