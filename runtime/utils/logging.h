/*
 * logging.h
 *
 *  Created on: Oct 16, 2014
 *      Author: kp
 */
#define BOOST_LOG_DYN_LINK

#ifndef UTILS_LOGGING_H_
#define UTILS_LOGGING_H_

#include <boost/log/trivial.hpp>
#include <iostream>
#include <thread>

#define LOG(level, msg) {/* BOOST_LOG_TRIVIAL(level) << msg;*/ std::cerr << msg << std::endl; }

#define DLOG(msg) {/* BOOST_LOG_TRIVIAL(level) << msg;*/ std::cerr << msg << std::endl; }

#define DLOGN(msg) {/* BOOST_LOG_TRIVIAL(level) << msg;*/ std::cerr << msg ; }

#define ASSERT(e) { assert(e); }

#endif /* UTILS_LOGGING_H_ */
