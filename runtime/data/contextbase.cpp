/*
 * contextbase.cpp
 *
 *  Created on: Mar 2, 2015
 *      Author: kp
 */

#include "data/contextbase.h"
#include "data/context.h"

namespace casm {

ContextBase::ContextBase(int maxTaskCount, int threadCount) :
		taskCount(0), maxTaskCount(maxTaskCount), threadCount(threadCount) {
}
ContextBase::~ContextBase() {
}

void ContextBase::stop() {
	std::unique_lock<std::mutex> m(lock);
	for (int i = 0; i < threadCount; ++i) {
		tasks.push_back(nullptr);
	}
	waitingForTask.notify_all();
}

void ContextBase::pushTask(std::unique_ptr<Context>&& ctx) {
	std::unique_lock<std::mutex> m(lock);
	tasks.push_back(std::move(ctx));
	++unsafeTaskCount;
	waitingForTask.notify_one();
}

void ContextBase::pushTasks(std::vector<std::unique_ptr<Context>>* contexts) {
	std::unique_lock<std::mutex> m(lock);
	for (auto& ctx : *contexts) {
		tasks.push_back(std::move(ctx));
		++unsafeTaskCount;
		waitingForTask.notify_one();
	}
}

std::unique_ptr<Context> ContextBase::popTask() {
	std::unique_lock<std::mutex> m(lock);
	while (tasks.size() <= 0) {
		++waitingThreads;
		waitingForTask.wait(m);
		--waitingThreads;
	}
	auto res = std::move(tasks.back());
	tasks.pop_back();
	--unsafeTaskCount;
	return std::move(res);
}

}

