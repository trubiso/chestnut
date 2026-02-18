CXX = g++
CXXFLAGS = `llvm-config --system-libs --cppflags --ldflags --libs core` -std=c++23 -lboost_program_options -Wall -Wextra -Wpedantic -g

OBJS = analyzer.o codegen.o diagnostic.o ir.o levenshtein.o lexer.o main.o out_fmt.o parser.o resolver.o resolver_desugar.o resolver_identify.o resolver_lower.o resolver_symbols.o resolver_types.o test.o token.o ast/expression.o ast/function.o ast/generics.o ast/identifier.o ast/module.o ast/statement.o ast/struct.o ast/tag.o ast/type.o

BUILD_DIR := build
SRC_DIR   := src

OBJS := $(addprefix $(BUILD_DIR)/,$(OBJS))
SRCS := $(OBJS:$(BUILD_DIR)/%.o=$(SRC_DIR)/%.cpp)

./chc: $(OBJS)
	$(CXX) $(CXXFLAGS) $^ -o ./chc
.PHONY: build

$(BUILD_DIR)/%.o: $(SRC_DIR)/%.cpp
	@mkdir -p $(dir $@)
	$(CXX) -c $(CXXFLAGS) -MMD -MP $< -o $@

.PHONY: clean cleanall build run test tidy cleancallgrind callgrind

clean:
	rm -rf $(BUILD_DIR) chc

cleanall:
	rm -rf $(BUILD_DIR) chc compile_commands.json

build: ./chc
run: ./chc
	./chc my_module
	gcc -o main output.o my_module_print_integer.c
	./main
test: ./chc
	./chc --run-compiler-tests

compile_commands.json:
	make cleanall
	bear -- make -j4

tidy: compile_commands.json
	clang-tidy -header-filter=.* $(SRCS)

cleancallgrind:
	rm -f vgcore.* callgrind.*

callgrind: ./chc cleancallgrind
	valgrind --dump-instr=yes --tool=callgrind ./chc my_module

-include $(OBJS:.o=.d)
