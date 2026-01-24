#pragma once
#include <cstdint>
#include <iostream>
#include <optional>
#include <string_view>

namespace AST {

struct Identifier {
	std::string_view        name;
	std::optional<uint32_t> id;

	explicit inline Identifier(std::string_view name, std::optional<uint32_t> id = {}) : name(name), id(id) {}
};

std::ostream& operator<<(std::ostream&, Identifier const&);

}  // namespace AST
