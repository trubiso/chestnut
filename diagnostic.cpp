#include "diagnostic.hpp"

#include <cstddef>
#include <cstdint>
#include <cstdlib>
#include <iostream>

#define TAB_STR "        "

constexpr OutFmt::Color severity_bg(Diagnostic::Severity severity) {
	switch (severity) {
	case Diagnostic::Severity::Error:   return OutFmt::Color::Red;
	case Diagnostic::Severity::Warning: return OutFmt::Color::Yellow;
	default:                            [[assume(false)]];
	}
}

constexpr OutFmt::Color severity_fg(Diagnostic::Severity severity) {
	switch (severity) {
	case Diagnostic::Severity::Error:   return OutFmt::Color::BrightWhite;
	case Diagnostic::Severity::Warning: return OutFmt::Color::Black;
	default:                            [[assume(false)]];
	}
}

constexpr char const* severity_name(Diagnostic::Severity severity) {
	switch (severity) {
	case Diagnostic::Severity::Error:   return "error";
	case Diagnostic::Severity::Warning: return "warning";
	default:                            [[assume(false)]];
	}
}

inline size_t number_size(size_t x) {
	return std::to_string(x).size();
}

void print_loc_line(size_t loc_pad, size_t loc_current = 0) {
	OutFmt::fg(OutFmt::Color::Gray);
	if (loc_current == 0) printf(" %*s │ ", (int) loc_pad, "");
	else { printf(" %*zu │ ", (int) loc_pad, loc_current); }
	OutFmt::reset();
}

Span Diagnostic::Sample::span() const {
	size_t min_start = SIZE_MAX, max_end = 0;
	for (Label const& label : labels) {
		if (label.span.start < min_start) min_start = label.span.start;
		if (label.span.end > max_end) max_end = label.span.end;
	}
	return Span(min_start, max_end);
}

struct SampleLocInfo {
	size_t loc_start, start_index, loc_end, end_index;
};

SampleLocInfo get_sample_loc_info(Span total_span, std::vector<size_t> const& loc, std::string_view code) {
	size_t loc_start = SIZE_MAX, start_index, loc_end = loc.size(), end_index = code.size();
	for (size_t i = 0; i < loc.size(); ++i) {
		size_t j = loc.size() - i - 1;
		if (loc_start == SIZE_MAX && loc[j] <= total_span.start) {
			loc_start   = j + 1;
			start_index = loc[j];
		}
	}
	for (size_t i = 0; i < loc.size(); ++i) {
		if (loc_end == loc.size() && loc[i] >= total_span.end) {
			loc_end   = i;
			end_index = loc[i] - 1;
		}
	}
	if (loc_start == SIZE_MAX) {
		std::cerr << "[diagnostics] failed to find line for index " << total_span.start << std::endl;
		std::exit(1);
	}
	return SampleLocInfo {loc_start, start_index, loc_end, end_index};
}

void Diagnostic::Sample::print() const {
	Span total_span = span();

	auto [loc_start, start_index, loc_end, end_index] = get_sample_loc_info(total_span, context.loc, context.source);

	size_t loc_pad     = number_size(loc_end);
	size_t loc_current = loc_start;

	putchar('\n');
	// TODO: print sample title
	OutFmt::fg(OutFmt::Color::Gray);
	size_t col_start = total_span.start - context.loc[loc_start - 1] + 1;
	std::cout << context.name << ':' << loc_start << ':' << col_start << '\n';
	OutFmt::fg_reset();
	print_loc_line(loc_pad);
	putchar('\n');

	// TODO: print label text
	for (size_t i = start_index; i < end_index; ++i) {
		if (i == start_index) print_loc_line(loc_pad, loc_current);
		if (context.source.at(i) == '\n') {
			putchar('\n');
			++loc_current;
			print_loc_line(loc_pad, loc_current);
		} else {
			if (context.source.at(i) == '\t') {
				printf("%s", TAB_STR);
				continue;
			}

			OutFmt::fg(OutFmt::Color::BrightWhite);
			for (Label const& label : labels) {
				if (i >= label.span.start && i < label.span.end) {
					OutFmt::fg(label.color);
					OutFmt::set_bold();
				}
			}

			putchar(context.source.at(i));
			OutFmt::reset();
		}
	}

	putchar('\n');
	print_loc_line(loc_pad);
	puts("\n");
}

void print_title(Diagnostic::Severity severity, std::string const& title) {
	OutFmt::bg(severity_bg(severity));
	OutFmt::fg(severity_fg(severity));
	OutFmt::set_bold();
	printf("%s:", severity_name(severity));
	OutFmt::bg_reset();
	OutFmt::fg(OutFmt::Color::BrightWhite);
	putchar(' ');
	puts(title.c_str());
	OutFmt::reset();
}

void Diagnostic::print() const {
	print_title(severity, title);

	if (subtitle.has_value()) {
		OutFmt::fg(OutFmt::Color::White);
		puts(subtitle.value().c_str());
		OutFmt::reset();
	}

	for (Sample const& sample : samples) { sample.print(); }
}
