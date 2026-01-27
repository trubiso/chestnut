CXX = g++
CXXFLAGS = -std=c++23 -Wall -Wextra -Wpedantic -g

OBJS = diagnostic.o levenshtein.o lexer.o main.o out_fmt.o parser.o resolver.o token.o ast/expression.o ast/function.o ast/identifier.o ast/module.o ast/statement.o ast/tag.o ast/type.o

./out: $(OBJS)
	$(CXX) $(CXXFLAGS) $^ -o ./out
.PHONY: build

%.o : %.cpp
	$(CXX) -c $(CXXFLAGS) $< -o $@

.PHONY: clean cleanall build run tidy cleancallgrind callgrind

clean:
	rm -f *.o **/*.o out

cleanall:
	rm -f *.o **/*.o out compile_commands.json

build: ./out
run: ./out
	./out

compile_commands.json:
	make cleanall
	bear -- make -j4

tidy: compile_commands.json
	clang-tidy -header-filter=.* $(OBJS:.o=.cpp)

cleancallgrind:
	rm -f vgcore.* callgrind.*

callgrind: ./out cleancallgrind
	valgrind --dump-instr=yes --tool=callgrind ./out
