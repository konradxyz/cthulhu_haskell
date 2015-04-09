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

int Context::intFromTemporary(unsigned id) const {
	//return static_cast<const IntValue*>(currentFrame->temporaryValues[id])->getValue();
	return 0;
}

Context::Context() :
		currentFrame(nullptr), nextInstruction(0) {
}


Context::~Context(){}

std::unique_ptr<seq::Context> generateStartingContext(int param, unsigned entryPoint, unsigned final, unsigned envSize) {
	auto result = utils::make_unique<seq::Context>();
	result->allocateFrame(0);
	result->currentFrame->nextInstruction = final;
	result->allocateFrame(envSize);
	result->currentFrame->environment[0] = seq::generateValueWrapper(utils::make_unique<seq::IntValue>(param));
	result->nextInstruction = entryPoint;
	return std::move(result);
}

}

