/*
 * futurecontext.h
 *
 *  Created on: Mar 2, 2015
 *      Author: kp
 */

#ifndef ASM_FUTURECONTEXT_H_
#define ASM_FUTURECONTEXT_H_

#include "static/utils/spinlock.h"
#include <memory>
#include <vector>

namespace casm {

class Context;
class ValueWrapper;

struct ContextUpdateRequest {
	std::unique_ptr<Context> context;
	unsigned source;
	unsigned target;

	ContextUpdateRequest(std::unique_ptr<Context>&& context, unsigned source,
			unsigned target);

	~ContextUpdateRequest();
};

// Note that objects of this class are essentially owned by ValueWrappers.
// ValueWrappers are managed by shared pointers.
// As such, we should not store FutureContext* anywhere, we should retrieve it from wrappers.
class FutureContext {
private:
	// Result - only real value wrappers are allowed here.
	// There is no static check - we need to be careful here.
	std::shared_ptr<ValueWrapper> valueWrapper;
	std::vector<std::unique_ptr<ContextUpdateRequest>> requests;
	utils::SpinLock lock;
	bool hasNoValue = true;
	unsigned rand = 1231231;
public:
	std::unique_ptr<Context> updateContext(std::unique_ptr<Context>&& context,
			unsigned source, unsigned target);

	void setValueAndWakeAll(std::shared_ptr<ValueWrapper>&& value);
	~FutureContext();
};

}

#endif /* ASM_FUTURECONTEXT_H_ */
