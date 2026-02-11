#include "struct.hpp"

namespace AST {

std::ostream& operator<<(std::ostream& os, Struct const& struct_) {
	os << "declare struct " << struct_.name.value << " w/ fields: {\n";
	os.iword(0)++;
	for (Struct::Field const& field : struct_.fields) {
		for (long i = 0; i < os.iword(0); ++i) os << "    ";
		std::cout << field.name.value << ": " << field.type.value << "\n";
	}
	os.iword(0)--;
	for (long i = 0; i < os.iword(0); ++i) os << "    ";
	return os << "}";
}

}  // namespace AST
