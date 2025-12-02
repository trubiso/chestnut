#include "diagnostic.hpp"

#include "out_fmt.hpp"

#include <algorithm>
#include <cstdint>
#include <cstdio>
#include <iostream>
#include <string>

#define TAB_STR "        "

constexpr OutFmt::Color severity_color(Diagnostic::Severity severity) {
	switch (severity) {
	case Diagnostic::Severity::Error:   return OutFmt::Color::Red;
	case Diagnostic::Severity::Warning: return OutFmt::Color::Yellow;
	case Diagnostic::Severity::Note:    return OutFmt::Color::Cyan;
	default:                            [[assume(false)]];
	}
}

constexpr char const* severity_name(Diagnostic::Severity severity) {
	switch (severity) {
	case Diagnostic::Severity::Error:   return "error";
	case Diagnostic::Severity::Warning: return "warning";
	case Diagnostic::Severity::Note:    return "note";
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

void Diagnostic::print(std::vector<size_t> const& loc, std::string_view code) const {
	// TODO: add an overall diagnostic span, make labels pointy and print the span
	// on the terminal

	// header
	OutFmt::bg(severity_color(severity));
	if (severity != Severity::Error) OutFmt::fg(OutFmt::Color::Black);
	else OutFmt::fg(OutFmt::Color::BrightWhite);
	OutFmt::set_bold();
	printf("%s:", severity_name(severity));
	OutFmt::bg_reset();
	if (severity != Severity::Error) OutFmt::fg(OutFmt::Color::BrightWhite);
	putchar(' ');
	printf("%s\n", title.c_str());
	OutFmt::reset();

	// subtitle
	if (subtitle.has_value()) {
		OutFmt::fg(OutFmt::Color::White);
		puts(subtitle.value().c_str());
		OutFmt::reset();
	}

	// labels
	if (!labels.empty()) {
		size_t start_l = SIZE_MAX, end_l = 0;
		for (auto const& label : labels) {
			if (label.span.start < start_l) start_l = label.span.start;
			if (label.span.end > end_l) end_l = label.span.end;
		}

		size_t loc_start = SIZE_MAX, start_index = 0, loc_end = loc.size(), end_index = code.size();
		for (size_t i = 0; i < loc.size(); ++i) {
			size_t j = loc.size() - i - 1;
			if (loc_start == SIZE_MAX && loc[j] <= start_l) {
				loc_start   = j + 1;
				start_index = loc[j];
			}
		}
		for (size_t i = 0; i < loc.size(); ++i) {
			if (loc_end == loc.size() && loc[i] >= end_l) {
				loc_end   = i;
				end_index = loc[i] - 1;
			}
		}

		if (loc_start == SIZE_MAX) {
			std::cerr << "[diagnostics] failed to find line for index " << start_l << std::endl;
			return;
		}

		size_t loc_pad     = number_size(loc_end);
		size_t loc_current = loc_start;

		putchar('\n');
		print_loc_line(loc_pad);
		putchar('\n');

		// TODO: print label text
		for (size_t i = start_index; i < end_index; ++i) {
			if (i == start_index) print_loc_line(loc_pad, loc_current);
			if (code.at(i) == '\n') {
				putchar('\n');
				++loc_current;
				print_loc_line(loc_pad, loc_current);
			} else {
				if (code.at(i) == '\t') {
					printf(TAB_STR);
					continue;
				}

				OutFmt::fg(OutFmt::Color::BrightWhite);
				for (Diagnostic::Label const& label : labels)
					if (i >= label.span.start && i < label.span.end) {
						OutFmt::fg(severity_color(label.color_override.value_or(severity)));
						OutFmt::set_bold();
					}
				putchar(code.at(i));
				OutFmt::reset();
			}
		}

		putchar('\n');
		print_loc_line(loc_pad);
		puts("\n");

		printf("%zu, %zu, %zu, %zu, %zu\n", number_size(end_index), loc_start, start_index, loc_end, end_index);
	}
}
