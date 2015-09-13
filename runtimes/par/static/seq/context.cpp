/*
 * context.cpp
 *
 *  Created on: Mar 2, 2015
 *      Author: kp
 */
#include "static/seq/data.h"

namespace seq {

Frame::Frame(unsigned envSize) :
		environment(envSize), nextInstruction(0) {
}
Frame::~Frame() {
}


Context::Context(TaskQueue* queue) :
		currentFrame(nullptr), nextInstruction(0), arithmeticAccumulator(0), taskQueue(queue) {
}


Context::~Context(){}

std::unique_ptr<seq::Context> generateStartingContext(int param, unsigned entryPoint,
		unsigned final, unsigned envSize, TaskQueue* queue, std::shared_ptr<FutureValue> res) {
	auto result = utils::make_unique<seq::Context>(queue);
	result->allocateFrame(1);
	result->currentFrame->environment[0] = res;
	result->currentFrame->nextInstruction = final;
	result->allocateFrame(envSize);
	result->currentFrame->environment[0] = std::make_shared<seq::IntValue>(param);
	result->nextInstruction = entryPoint;
	return std::move(result);
}

}

