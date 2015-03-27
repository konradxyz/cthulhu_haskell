/*
 * value.cpp
 *
 *  Created on: Feb 17, 2015
 *      Author: kp
 */

#include "data/value.h"
#include "data/futurecontext.h"
#include "utils/ptr.h"

namespace casm {
std::shared_ptr<ValueWrapper> generateFutureWrapper() {
	return std::make_shared<ValueWrapper>(
			utils::make_unique<FutureContext>());
}
std::shared_ptr<ValueWrapper> generateValueWrapper(
		std::unique_ptr<Value>&& value) {
	return std::make_shared<ValueWrapper>(std::move(value));
}
ValueWrapper::ValueWrapper(std::unique_ptr<FutureContext>&& future) :
		type(FUTURE_VALUE), value(nullptr), future(std::move(future)) {
}
ValueWrapper::ValueWrapper(std::unique_ptr<Value>&& value) :
		type(REAL_VALUE), value(std::move(value)), future(nullptr) {
}

ValueWrapper::~ValueWrapper() {}

}
