#include<iostream>
#include<thread>
#include<vector>

unsigned thread_count = 1;
unsigned count = 100000;


int operation() {
  int res = 0;
  for ( unsigned i = 0; i < count; i++ ) {
    res += count;
  }
  return res;
}




int main(int argc, char* args[]) {
  thread_count = atoi(args[1]);
  count = atoi(args[2]);
  std::cout << "Thread count " << thread_count << std::endl;
  std::cout << "count " << count << std::endl;
  std::vector<std::thread> threads;
  for ( unsigned i = 0; i < thread_count; ++i )
    threads.emplace_back(operation);
  for ( unsigned i = 0; i < threads.size(); ++i )
    threads[i].join();
  std::cout << "Done" << std::endl;
}
