#include <assert.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>

FILE* open_input() {
	FILE* file = fopen("aoc_2025_01.input", "r");
	assert(file);
	return file;
}

void close_input(FILE* file) {
	fclose(file);
}

char     buf[16];
bool     direction;
uint32_t rotation;

bool get_line(FILE* file) {
	if (fgets(buf, 16, file) == NULL) return false;
	direction = buf[0] == 'R';
	sscanf(buf + 1, "%u", &rotation);
	return true;
}

bool get_direction() {
	return direction;
}

uint32_t get_rotation() {
	return rotation;
}

void print_output(uint64_t value) {
	printf("%lu\n", value);
}
