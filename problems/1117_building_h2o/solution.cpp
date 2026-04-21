// Leetcode 1117. Building H2O

#include <functional>
#include <thread>

class H2O {
 private:
  int h_count_;

  // Thread control
  std::mutex mtx_;
  std::condition_variable cv_;

 public:
  H2O() {}

  void hydrogen(std::function<void()> releaseHydrogen) {
    std::unique_lock<std::mutex> lock(mtx_);

    cv_.wait(lock, [this] { return h_count_ < 2; });

    // releaseHydrogen() outputs "H". Do not change or remove this line.
    releaseHydrogen();

    ++h_count_;
    cv_.notify_all();
  }

  void oxygen(std::function<void()> releaseOxygen) {
    std::unique_lock<std::mutex> lock(mtx_);

    cv_.wait(lock, [this] { return h_count_ == 2; });

    // releaseOxygen() outputs "O". Do not change or remove this line.
    releaseOxygen();

    h_count_ = 0;
    cv_.notify_all();
  }
};
