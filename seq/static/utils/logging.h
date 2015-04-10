/*
 * logging.h
 *
 *  Created on: Oct 16, 2014
 *      Author: kp
 */

#ifndef UTILS_LOGGING_H_
#define UTILS_LOGGING_H_

#include <iostream>


#define LOG(level, msg) {/* BOOST_LOG_TRIVIAL(level) << msg;*/ std::cerr << msg << std::endl; }

#define DLOG(msg) {/* BOOST_LOG_TRIVIAL(level) << msg;*/ std::cerr << "DLOG: " <<  msg << std::endl; }

#define DLOGN(msg) {/* BOOST_LOG_TRIVIAL(level) << msg;*/ std::cerr << msg ; }

#define ASSERT(e) { assert(e); }

#endif /* UTILS_LOGGING_H_ */
