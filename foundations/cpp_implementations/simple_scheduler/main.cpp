// Compile:
//   g++ -std=c++17 -DSINGLE_THREAD -o scheduler main.cpp && ./scheduler

#define MULTI_THREAD
#include <chrono>
#include <iostream>

#include "scheduler.cpp"

using ms = std::chrono::milliseconds;

int main() {
  TaskScheduler scheduler;

  scheduler.schedule(ms{5000}, []() -> bool {
    std::cout << "A - 5s\n";
    return false;
  });
  scheduler.schedule(ms{7000}, []() -> bool {
    std::cout << "B - 7s\n";
    return false;
  });
  scheduler.schedule(ms{12000}, []() -> bool {
    std::cout << "C - 12s\n";
    return false;
  });

  scheduler.worker_loop();
  return 0;
}
