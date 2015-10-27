/*
 * taskqueue.cpp
 *
 *  Created on: May 3, 2015
 *      Author: kp
 */

#include "static/seq/taskqueue.h"
#include "static/seq/value.h"

namespace seq {
#ifdef NO_MUTEX
#define QUEUE_LOCK std::unique_lock<utils::SpinLock> m(spin_lock);
#define QUEUE_MUTEX(x) ;
#else
#define QUEUE_LOCK std::unique_lock<std::mutex> m(lock);
#define QUEUE_MUTEX(x) x;
#endif

#define QUEUE_NOTIFY QUEUE_MUTEX( waiting.notify_one() )

void TaskQueue::pushTask(std::unique_ptr<Context>&& ctx) {
  QUEUE_LOCK
  contexts.push_back(std::move(ctx));
  ++taskCount;
  QUEUE_NOTIFY
}

void TaskQueue::pushTasks(std::vector<std::unique_ptr<Context>>* contexts) {
  QUEUE_LOCK
	for (auto& ctx : *contexts) {
		this->contexts.push_back(std::move(ctx));
		++taskCount;
		QUEUE_NOTIFY
	}
}

std::unique_ptr<Context> TaskQueue::popTask() {
  #ifdef NO_MUTEX
  QUEUE_LOCK
  ++waitingThreads;
  while (contexts.size() <= 0) {
    if (!finishing && waitingThreads >= threadCount) {
      finishing = true;
      for (int i = 0; i < threadCount; ++i) {
        contexts.push_back(nullptr);
      }
    }
    m.unlock();
    m.lock();
  }
  --waitingThreads;
  auto res = std::move(contexts.back());
  contexts.pop_back();
  --taskCount;
  return std::move(res);
  #else
  QUEUE_LOCK
  while (contexts.size() <= 0) {
    ++waitingThreads;
    if (waitingThreads >= threadCount) {
      for (int i = 0; i < threadCount; ++i) {
        contexts.push_back(nullptr);
      }
      QUEUE_MUTEX(waiting.notify_all())
    } else {
      QUEUE_MUTEX(waiting.wait(m))
    }
    --waitingThreads;
  }
  auto res = std::move(contexts.back());
  contexts.pop_back();
  --taskCount;
  return std::move(res);
  #endif
}

TaskQueue::TaskQueue(int maxTaskCount, int threadCount) :
		taskCount(0), maxTaskCount(maxTaskCount), threadCount(threadCount), result(std::make_shared<FutureValue>()) {
}

TaskQueue::~TaskQueue() {}
}
