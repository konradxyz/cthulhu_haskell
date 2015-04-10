/*
 * error.h
 *
 *  Created on: Mar 21, 2015
 *      Author: kp
 */

#ifndef UTILS_ERROR_H_
#define UTILS_ERROR_H_

#include <string>
#include <stdexcept>

namespace utils {



class CthulhuException : public std::runtime_error {
public:
	CthulhuException(const std::string& msg) : std::runtime_error(msg) {}
};

void error(const std::string& msg);


}


#endif /* UTILS_ERROR_H_ */
