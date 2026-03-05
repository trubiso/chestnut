#include <string>
#include <string_view>
#include <vector>

size_t levenshtein(std::string_view, std::string_view);
std::vector<size_t> multi_levenshtein(std::string_view, std::vector<std::string> const&);
std::vector<std::string_view> closest(std::string_view, std::vector<std::string> const&, size_t levenshtein_threshold, size_t max = 0);
