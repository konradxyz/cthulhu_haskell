
#include "utils/error.h"

namespace utils {


void error(const std::string& msg) {
	throw CthulhuException(msg);
}

}
