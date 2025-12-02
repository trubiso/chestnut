CXX = g++
CXXFLAGS = -std=c++23 -Wall -Wpedantic -g -O3

OBJS = diagnostic.o lexer.o main.o out_fmt.o token.o

./out: $(OBJS)
	$(CXX) $(CXXFLAGS) $^ -o ./out
.PHONY: build

%.o : %.cpp
	$(CXX) -c $(CXXFLAGS) $< -o $@

.PHONY: clean build run tidy cleancallgrind callgrind

clean:
	rm -f *.o **/*.o out compile_commands.json

build: ./out
run: ./out
	./out

compile_commands.json:
	make clean
	bear -- make -j4

tidy: compile_commands.json
	clang-tidy -header-filter=.* $(OBJS:.o=.cpp)

cleancallgrind:
	rm -f vgcore.* callgrind.*

callgrind: ./out cleancallgrind
	valgrind --dump-instr=yes --tool=callgrind ./out
