#pragma once
#include <optional>
#include <string_view>

template <typename T>
class Stream {
public:
	explicit Stream(std::basic_string_view<T> data) : data_(data), index_(0) {}

	/// Returns the item at a particular position if it exists.
	inline std::optional<T> at(size_t index) const noexcept {
		return reaches(index) ? data_.at(index) : std::optional<T> {};
	}

	/// Returns the item at the current position if it exists.
	inline std::optional<T> peek() const noexcept { return at(index_); }

	/// Returns the last item in the stream, throwing if the stream is empty.
	inline T const& last() const noexcept(!empty()) { return data_.at(size() - 1); }

	/// Return the result of peek(), advancing if it is nonnull.
	inline std::optional<T> consume() {
		auto value = peek();
		if (value.has_value()) advance();
		return value;
	}

	/// Sets the index to the provided index.
	inline void set_index(size_t new_index) { index_ = new_index; }

	/// Advances by the provided amount, returning whether the new index is valid
	/// or not.
	inline bool advance(size_t amount = 1) {
		index_ += amount;
		return has_value();
	}

	/// Retreats by the provided amount, returning whether the new index is valid
	/// or not.
	inline bool retreat(size_t amount = 1) {
		index_ -= amount;
		return has_value();
	}

	/// Returns whether the stream has a value at the provided index.
	inline bool reaches(size_t index) const { return index < size(); }

	/// Returns whether the stream has a value at the current position.
	inline bool has_value() const { return reaches(index()); }

	/// Returns the current index.
	inline size_t index() const { return index_; }

	/// Returns the length.
	inline size_t size() const { return data_.size(); }

	/// Returns whether the stream is empty.
	constexpr inline bool empty() const { return size() == 0; }

	/// Returns the inner data.
	inline std::basic_string_view<T> const& data() const { return data_; }

private:
	std::basic_string_view<T> data_;
	size_t                    index_;
};
