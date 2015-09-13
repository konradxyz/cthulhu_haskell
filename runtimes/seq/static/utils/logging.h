/*
 * logging.h
 *
 *  Created on: Oct 16, 2014
 *      Author: kp
 */

#ifndef UTILS_LOGGING_H_
#define UTILS_LOGGING_H_

#include <iostream>


#define DLOG(msg) { std::cerr << "DLOG: " <<  msg << std::endl; }


#define ASSERT(e) { assert(e); }

#endif /* UTILS_LOGGING_H_ */
