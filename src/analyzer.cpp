#include "analyzer.hpp"

void Analyzer::analyze() {
	check_assigned();
}

void Analyzer::check_assigned() {
	std::cout << "we are running the analyzer!" << std::endl;
}
