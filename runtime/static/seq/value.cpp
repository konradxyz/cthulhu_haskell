/*
 * value.cpp
 *
 *  Created on: Feb 17, 2015
 *      Author: kp
 */

#include "static/seq/data.h"
#include "static/utils/ptr.h"

namespace seq {

std::shared_ptr<ValueWrapper> generateValueWrapper(
		std::unique_ptr<Value>&& value) {
	return std::make_shared<ValueWrapper>(std::move(value));
}

ValueWrapper::ValueWrapper(std::unique_ptr<Value>&& value) :
		type(REAL_VALUE), value(std::move(value)) {
}

ValueWrapper::~ValueWrapper() {}
CallSpecification::~CallSpecification(){}

}
