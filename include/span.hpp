#pragma once
#include <cassert>
#include <cstddef>

struct Span {
	size_t start, end;

	explicit Span(size_t start, size_t end) : start(start), end(end) { assert(start <= end); }

	explicit Span(size_t pos) : start(pos), end(pos + 1) {}

	static Span zero() { return Span(0); }

	bool operator==(Span const& other) const = default;
};

template <typename T>
struct Spanned {
	Span span;
	T    value;
};
