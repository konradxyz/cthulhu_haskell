#include "gen.h"
#include "static/seq/seq.h"
#include "static/utils/args.h"
#include <iostream>
#include <cstdlib>
#include <thread>

std::unique_ptr<seq::Context> generateStartingContext(int param, seq::TaskQueue* queue, std::shared_ptr<seq::FutureValue> result) {
	return seq::generateStartingContext(param, START_LABEL, FINAL_LABEL, START_ENV_SIZE, queue, result);
}

std::unique_ptr<seq::TaskQueue> generateQueue(int param, int thread_count) {
	auto res = utils::make_unique<seq::TaskQueue>(5 * thread_count, thread_count);
	auto ctx = generateStartingContext(param, res.get(), res->getResult());
	res->pushTask(std::move(ctx));
	return res;
}


void executeQueue(seq::TaskQueue* queue) {
	auto task = queue->popTask();
	while (task != nullptr ) {
		executeContext(std::move(task));
		task = queue->popTask();
	}
}


int main(int argc, char *argv[]) {
    utils::Options opts;
    opts.parse(argc, argv);
    int param = opts.integer(0);
    int thread_count = opts.integer("threads");
    if ( thread_count < 1 ) {
      std::cerr << "Thread count should be greater than 0" << std::endl;
      return 1;
    }
    std::cerr << "Param: " << param << ", threads: " << thread_count << std::endl;
    auto base = generateQueue(param, thread_count);
    std::vector<std::thread> thread;
    for (int i = 0; i < thread_count; ++i ) {
      thread.emplace_back(executeQueue, base.get());
    }
    for ( auto& t : thread ) {
      t.join();
    }
    std::cout << INT(base->getResult()->getFuture()) << std::endl;
    return 0;
}
