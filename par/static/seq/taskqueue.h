/*
 * taskqueue.h
 *
 *  Created on: May 3, 2015
 *      Author: kp
 */

#ifndef STATIC_SEQ_TASKQUEUE_H_
#define STATIC_SEQ_TASKQUEUE_H_

#include "static/seq/context.h"
#include <vector>
#include <mutex>
#include <atomic>
#include <condition_variable>

namespace seq {
class FutureValue;

class TaskQueue {
private:
	std::vector<std::unique_ptr<seq::Context>> contexts;
	std::mutex lock;
	std::condition_variable waiting;
	std::atomic_int taskCount;
	const int maxTaskCount; // This is only a hint, we will not enforce it.
	const int threadCount;
	std::shared_ptr<FutureValue> result;
	int waitingThreads = 0;
public:
	void pushTask(std::unique_ptr<Context>&& ctx);
	void pushTasks(std::vector<std::unique_ptr<Context>>* contexts);

	std::unique_ptr<Context> popTask();

	TaskQueue(int maxTaskCount, int threadCount);

	~TaskQueue();

	std::shared_ptr<FutureValue> getResult() {
		return result;
	}

	bool isFull() {
		return taskCount >= maxTaskCount;
	}
};

}



#endif /* STATIC_SEQ_TASKQUEUE_H_ */
