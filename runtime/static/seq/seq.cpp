#include "gen.h"
#include <iostream>
#include <cstdlib>

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
