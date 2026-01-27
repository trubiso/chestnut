#include "levenshtein.hpp"

#include <algorithm>

inline size_t min(size_t a, size_t b, size_t c) {
	return std::min(a, std::min(b, c));
}

// adapted from https://www.geeksforgeeks.org/dsa/introduction-to-levenshtein-distance/
size_t levenshtein(std::string_view str1, std::string_view str2) {
	std::vector<size_t> prevRow(str2.length() + 1), currRow(str2.length() + 1);

	for (int j = 0; j <= str2.length(); j++) prevRow[j] = j;

	for (int i = 1; i <= str1.length(); i++) {
		currRow[0] = i;

		for (int j = 1; j <= str2.length(); j++)
			currRow[j] = (str1[i - 1] == str2[j - 1])
			                   ? prevRow[j - 1]
			                   : (1 + min(currRow[j - 1], prevRow[j], prevRow[j - 1]));

		prevRow = currRow;
	}

	return currRow[str2.length()];
}

std::vector<size_t> multi_levenshtein(std::string_view original, std::vector<std::string> const& strings) {
	std::vector<size_t> results(strings.size());
	for (size_t i = 0; i < strings.size(); ++i) results[i] = levenshtein(original, strings[i]);
	return results;
}

std::vector<std::string_view>
closest(std::string_view original, std::vector<std::string> const& strings, size_t levenshtein_threshold, size_t max) {
	struct Entry {
		std::string_view string;
		size_t           distance;
	};

	std::vector<size_t> distances = multi_levenshtein(original, strings);
	std::vector<Entry>  entries(strings.size());
	for (size_t i = 0; i < strings.size(); ++i) entries[i] = {strings[i], distances[i]};
	std::ranges::sort(entries, {}, &Entry::distance);
	std::vector<std::string_view> results {};
	size_t                        actual_max = max == 0 ? entries.size() : max;
	for (size_t i = 0; i < std::min(entries.size(), actual_max); ++i) {
		if (entries[i].distance > levenshtein_threshold) break;
		results.push_back(entries[i].string);
	}

	return results;
}
