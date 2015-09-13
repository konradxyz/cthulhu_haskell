/*
 * taskqueue.cpp
 *
 *  Created on: May 3, 2015
 *      Author: kp
 */

#include "static/seq/taskqueue.h"
#include "static/seq/value.h"

namespace seq {

void TaskQueue::pushTask(std::unique_ptr<Context>&& ctx) {
	std::unique_lock<std::mutex> m(lock);
	contexts.push_back(std::move(ctx));
	++taskCount;
	waiting.notify_one();
}

void TaskQueue::pushTasks(std::vector<std::unique_ptr<Context>>* contexts) {
	std::unique_lock<std::mutex> m(lock);
	for (auto& ctx : *contexts) {
		this->contexts.push_back(std::move(ctx));
		++taskCount;
		waiting.notify_one();
	}
}

std::unique_ptr<Context> TaskQueue::popTask() {
	std::unique_lock<std::mutex> m(lock);
	while (contexts.size() <= 0) {
		++waitingThreads;
		if (waitingThreads >= threadCount) {
			for (int i = 0; i < threadCount; ++i) {
				contexts.push_back(nullptr);
			}
			waiting.notify_all();
		} else {
			waiting.wait(m);
		}
		--waitingThreads;
	}
	auto res = std::move(contexts.back());
	contexts.pop_back();
	--taskCount;
	return std::move(res);
}

TaskQueue::TaskQueue(int maxTaskCount, int threadCount) :
		taskCount(0), maxTaskCount(maxTaskCount), threadCount(threadCount), result(std::make_shared<FutureValue>()) {
}

TaskQueue::~TaskQueue() {}
}
