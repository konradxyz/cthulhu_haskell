/*
 * context.h
 *
 *  Created on: Feb 19, 2015
 *      Author: kp
 */

#ifndef ASM_CONTEXT_H_
#define ASM_CONTEXT_H_

#include "static/utils/ptr.h"
#include <vector>

namespace seq {

class ValueWrapper;

struct Frame {
	std::vector<std::shared_ptr<ValueWrapper>> environment;
	unsigned nextInstruction;

	Frame(unsigned envSize);
	~Frame();
};

struct Context {
	std::vector<std::unique_ptr<Frame>> frames;
	Frame* currentFrame;
	unsigned nextInstruction;
	std::shared_ptr<ValueWrapper> accumulator;
public:
	Context();
	~Context();

	// This one should be removed as well probably.
	void allocateFrame(unsigned envSize) {
		pushFrame(utils::make_unique<Frame>(envSize));
	}

	void pushFrame(std::unique_ptr<Frame>&& frame) {
		frames.push_back(std::move(frame));
		currentFrame = frames.back().get();
	}

	void removeLastFrame() {
		frames.pop_back();
		currentFrame = frames.back().get();
		nextInstruction = currentFrame->nextInstruction;
	}

	int intFromTemporary(unsigned id) const;
};

std::unique_ptr<seq::Context> generateStartingContext(int param,
		unsigned entryPoint, unsigned final, unsigned envSize);
}

#endif /* ASM_CONTEXT_H_ */
