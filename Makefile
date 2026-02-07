CXX = g++
CXXFLAGS = `llvm-config --system-libs --cppflags --ldflags --libs core` -std=c++23 -Wall -Wextra -Wpedantic -g

OBJS = codegen.o diagnostic.o ir.o levenshtein.o lexer.o main.o out_fmt.o parser.o resolver.o resolver_identify.o resolver_lower.o resolver_symbols.o resolver_types.o test.o token.o ast/expression.o ast/function.o ast/identifier.o ast/module.o ast/statement.o ast/tag.o ast/type.o

BUILD_DIR := build
SRC_DIR   := src

OBJS := $(addprefix $(BUILD_DIR)/,$(OBJS))
SRCS := $(OBJS:$(BUILD_DIR)/%.o=$(SRC_DIR)/%.cpp)

./out: $(OBJS)
	$(CXX) $(CXXFLAGS) $^ -o ./out
.PHONY: build

$(BUILD_DIR)/%.o: $(SRC_DIR)/%.cpp
	@mkdir -p $(dir $@)
	$(CXX) -c $(CXXFLAGS) $< -o $@

.PHONY: clean cleanall build run tidy cleancallgrind callgrind

clean:
	rm -rf $(BUILD_DIR) out

cleanall:
	rm -rf $(BUILD_DIR) out compile_commands.json

build: ./out
run: ./out
	./out

compile_commands.json:
	make cleanall
	bear -- make -j4

tidy: compile_commands.json
	clang-tidy -header-filter=.* $(SRCS)

cleancallgrind:
	rm -f vgcore.* callgrind.*

callgrind: ./out cleancallgrind
	valgrind --dump-instr=yes --tool=callgrind ./out
