/*
 * futurecontext.cpp
 *
 *  Created on: Mar 2, 2015
 *      Author: kp
 */

#include "data/futurecontext.h"
#include "utils/logging.h"
#include "data/context.h"
#include "data/value.h"


namespace casm {
unsigned rand = 1231231;

ContextUpdateRequest::ContextUpdateRequest(std::unique_ptr<Context>&& context,
		unsigned source, unsigned target) :
		context(std::move(context)), source(source), target(target) {
}

ContextUpdateRequest::~ContextUpdateRequest(){}

std::unique_ptr<Context> FutureContext::updateContext(
		std::unique_ptr<Context>&& context, unsigned source, unsigned target) {
	std::lock_guard<utils::SpinLock> guard(lock);
	ASSERT(rand == 1231231);
	if (!hasNoValue) {
		auto frame = context->currentFrame;
		// OK, this one is tricky - when using shared ptr.
		// Under source index there is some ValueWrapper.
		// That certainly owns this.
		// As such, we need to keep referene to this thing untill the end of the method.
		auto tmp = std::move(frame->environment[source]);
		frame->environment[source] = valueWrapper;
		//ASSERT(valueWrapper->getValue() != (Value*) 0x65);
		return std::move(context);
	} else {
		requests.push_back(utils::make_unique<ContextUpdateRequest>(std::move(context), source, target));
		return nullptr;
	}

}

void FutureContext::setValueAndWakeAll(std::shared_ptr<ValueWrapper>&& value) {
	std::lock_guard<utils::SpinLock> guard(lock);
	valueWrapper = std::move(value);
	std::vector<std::unique_ptr<Context>> contexts;
	hasNoValue = false;
	for (auto& request : requests) {
		auto frame = request->context->currentFrame;
		frame->environment[request->source] = valueWrapper;
		contexts.emplace_back(std::move(request->context));
	}
}

FutureContext::~FutureContext(){}

}


