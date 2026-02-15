#include <stdint.h>
#include <stdio.h>

void print_bool(unsigned char bool_) {
	if (bool_) printf("true\n");
	else printf("false\n");
}

void print_integer(uint64_t value) {
	printf("%lu", value);
}
