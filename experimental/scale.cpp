#include<iostream>
#include<thread>
#include<vector>
//
unsigned thread_count = 1;
unsigned count = 100000;
//
//
//int operation() {
//  int res = 0;
//  for ( unsigned i = 0; i < count; i++ ) {
//    res += count;
//  }
//  return res;
//}
//

template<typename A, typename B, typename F>
void map(F& f, std::vector<A>& input, std::vector<B>& output) {
  output.resize(input.size());
  for ( unsigned i = 0; i < input.size(); ++i ){
    output[i] = f(input[i]);
  }
}


template<typename A, typename B, typename F>
void map_par(F& f, std::vector<A>& input, std::vector<B>& output) {
  output.resize(input.size());
  std::vector<std::thread> threads;
  auto operation = [&input, &output, &f](int i) {
    output[i] = f(input[i]);
  };
  for ( unsigned i = 0; i < input.size(); ++i )
    threads.emplace_back(operation, i);
  for ( unsigned i = 0; i < threads.size(); ++i )
    threads[i].join();
}



int inc(int i) {
  return i + 1;
}

int main(int argc, char* args[]) {
    std::cout << "Yoo madafaka" << std::endl;
    std::vector<int> input = {1, 2, 3, 4};
    std::vector<int> output;
    map_par(inc, input, output);
    for ( unsigned i = 0; i < output.size(); ++i ) {
      std::cout << output[i] << std::endl;
    }
 // thread_count = atoi(args[1]);
 // count = atoi(args[2]);
 // std::cout << "Thread count " << thread_count << std::endl;
 // std::cout << "count " << count << std::endl;
//  std::vector<std::thread> threads;
//  for ( unsigned i = 0; i < thread_count; ++i )
//    threads.emplace_back(operation);
//  for ( unsigned i = 0; i < threads.size(); ++i )
//    threads[i].join();
//  std::cout << "Done" << std::endl;
}
