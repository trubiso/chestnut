#pragma once
#include <cwchar>
#include <optional>
#include <string_view>
#include <type_traits>
#include <vector>

template <typename T>
class Stream {
public:
	using container_t = std::conditional_t<std::is_same_v<T, char>, std::string_view, std::vector<T>>;

	explicit Stream(container_t&& data) : data_(data), index_(0) {}

	/// Returns the item at a particular position if it exists.
	inline std::optional<T> at(size_t index) const noexcept {
		return reaches(index) ? data_.at(index) : std::optional<T> {};
	}

	/// Returns the item at the current position if it exists.
	inline std::optional<T> peek() const noexcept { return at(index_); }

	/// Returns the last item in the stream, throwing if the stream is empty.
	inline T const& last() const { return data_.at(size() - 1); }

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

	/// Consumes while the predicate returns true for the current value.
	inline void consume_while(std::predicate<T> auto condition) {
		for (auto current = peek(); current.has_value() && condition(current.value());
		     advance(), current = peek());
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
	inline bool empty() const { return size() == 0; }

	/// Returns the inner data.
	inline std::basic_string_view<T> const& data() const { return data_; }

private:
	container_t data_;
	size_t      index_;
};
