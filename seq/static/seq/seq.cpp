#include "gen.h"
#include "static/seq/seq.h"
#include <iostream>
#include <cstdlib>


std::unique_ptr<seq::Context> generateStartingContext(int param) {
	return seq::generateStartingContext(param, START_LABEL, FINAL_LABEL, START_ENV_SIZE);
}


int main(int argc, char *argv[]) {
	if ( argc < 2 ) {
		std::cerr << "Param expected" << std::endl;
		return 1;
	}
	int param = atoi(argv[1]);
	auto base = generateStartingContext(param);
	executeContext(base.get());
	std::cout << INT(base->accumulator) << std::endl;
	return 0;
}
