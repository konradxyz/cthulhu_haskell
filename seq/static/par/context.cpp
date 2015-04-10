/*
 * context.cpp
 *
 *  Created on: Mar 2, 2015
 *      Author: kp
 */
#include "static/data.h"

namespace casm {

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

}

