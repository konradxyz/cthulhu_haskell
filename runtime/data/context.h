/*
 * context.h
 *
 *  Created on: Feb 19, 2015
 *      Author: kp
 */

#ifndef ASM_CONTEXT_H_
#define ASM_CONTEXT_H_

#include "utils/ptr.h"
#include <vector>
#include "utils/spinlock.h"
#include <mutex>

namespace casm {

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
public:
	Context();
	~Context();

	// This one should be removed as well probably.
	void allocateFrame(unsigned envSize, unsigned tmpSize) {
		pushFrame(utils::make_unique<Frame>(envSize));
	}

	void pushFrame(std::unique_ptr<Frame>&& frame) {
		frames.push_back(std::move(frame));
		currentFrame = frames.back().get();
	}

	void removeLastFrame() {
		frames.pop_back();
		currentFrame = frames.back().get();
	}

	int intFromTemporary(unsigned id) const;
};

}


#endif /* ASM_CONTEXT_H_ */
