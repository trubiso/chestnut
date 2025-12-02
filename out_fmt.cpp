#include "out_fmt.hpp"
#ifdef WINDOWS
#include <Windows.h>
#else
#include <cstdint>
#include <iostream>
#endif

namespace OutFmt {

#ifdef WINDOWS
void set_color_inner(context, WORD color) {
	HANDLE h_console = GetStdHandle(STD_OUTPUT_HANDLE);
	SetConsoleTextAttribute(h_console, color);
}

void color(Context context, Color color) {
	using enum Color;
	switch (color) {
	case Black:         set_color_inner(context, +0); break;
	case Red:           set_color_inner(context, +1); break;
	case Green:         set_color_inner(context, +2); break;
	case Yellow:        set_color_inner(context, +3); break;
	case Blue:          set_color_inner(context, +4); break;
	case Magenta:       set_color_inner(context, +5); break;
	case Cyan:          set_color_inner(context, +6); break;
	case White:         set_color_inner(context, +7); break;
	case Gray:          set_color_inner(context, +8); break;
	case BrightRed:     set_color_inner(context, 12); break;
	case BrightGreen:   set_color_inner(context, 10); break;
	case BrightYellow:  set_color_inner(context, 14); break;
	case BrightBlue:    set_color_inner(context, +9); break;
	case BrightMagenta: set_color_inner(context, 13); break;
	case BrightCyan:    set_color_inner(context, 11); break;
	case BrightWhite:   set_color_inner(context, 15); break;
	}
}
#else
void set_color_inner(Context context, uint8_t color) {
	if (context == Context::Foreground) std::cout << "\033[38;5;" << (int32_t) color << 'm';
	else std::cout << "\033[48;5;" << (int32_t) color << 'm';
}

void color(Context context, Color color) {
	using enum Color;
	switch (color) {
	case Black:         set_color_inner(context, 0); break;
	case Red:           set_color_inner(context, 1); break;
	case Green:         set_color_inner(context, 2); break;
	case Yellow:        set_color_inner(context, 3); break;
	case Blue:          set_color_inner(context, 4); break;
	case Magenta:       set_color_inner(context, 5); break;
	case Cyan:          set_color_inner(context, 6); break;
	case White:         set_color_inner(context, 7); break;
	case Gray:          set_color_inner(context, 8); break;
	case BrightRed:     set_color_inner(context, 9); break;
	case BrightGreen:   set_color_inner(context, 10); break;
	case BrightYellow:  set_color_inner(context, 11); break;
	case BrightBlue:    set_color_inner(context, 12); break;
	case BrightMagenta: set_color_inner(context, 13); break;
	case BrightCyan:    set_color_inner(context, 14); break;
	case BrightWhite:   set_color_inner(context, 15); break;
	}
}

void set_bold() {
	std::cout << "\033[1m";
}

void clear_bold() {
	std::cout << "\033[22m";
}

void reset() {
	std::cout << "\033[0m";
}
#endif

void color_reset(Context context) {
	if (context == Context::Foreground) color(context, Color::White);
	else color(context, Color::Black);
}

}  // namespace OutFmt
