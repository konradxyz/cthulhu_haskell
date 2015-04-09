/*
 * contextbase.h
 *
 *  Created on: Mar 1, 2015
 *      Author: kp
 */

#ifndef ASM_CONTEXTBASE_H_
#define ASM_CONTEXTBASE_H_

#include <memory>
#include <vector>
#include "utils/spinlock.h"
#include <atomic>
#include <condition_variable>
#include <mutex>


namespace casm {

class Context;

class ContextBase {
private:
	std::atomic_int taskCount;
	int unsafeTaskCount = 0;
	const int maxTaskCount; // This is only a hint, we will not enforce it.
	const int threadCount;
	int waitingThreads = 0;
	// Always taskCount == tasks.size()
	std::vector<std::unique_ptr<Context>> tasks;
	std::mutex lock;
	std::condition_variable waitingForTask;
public:
	ContextBase(int maxTaskCount, int threadCount);
	bool isFull() {
		return unsafeTaskCount >= maxTaskCount;
	}

	void stop();

	void pushTask(std::unique_ptr<Context>&& ctx);
	void pushTasks(std::vector<std::unique_ptr<Context>>* contexts);

	std::unique_ptr<Context> popTask();
	~ContextBase();
};

}


#endif /* ASM_CONTEXTBASE_H_ */
