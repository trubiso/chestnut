#pragma once

namespace OutFmt {

// TODO: explicitly define which output stream to use

enum class Color {
	Black,
	Red,
	Green,
	Yellow,
	Blue,
	Magenta,
	Cyan,
	White,
	Gray,
	BrightRed,
	BrightGreen,
	BrightYellow,
	BrightBlue,
	BrightMagenta,
	BrightCyan,
	BrightWhite,
};

enum class Context {
	Foreground,
	Background,
};

void color(Context, Color);

inline void fg(Color c) {
	color(Context::Foreground, c);
}

inline void bg(Color c) {
	color(Context::Background, c);
}

void color_reset(Context);

inline void fg_reset() {
	color_reset(Context::Foreground);
}

inline void bg_reset() {
	color_reset(Context::Background);
}

void set_bold();
void clear_bold();

void reset();

}  // namespace OutFmt
