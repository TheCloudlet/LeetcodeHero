// Leetcode 1188. Design Bounded Blocking Queue

#include <queue>
#include <thread>

class BoundedBlockingQueue {
 private:
  std::queue<int> queue_;
  int capacity_;

  // thread control
  std::mutex mtx_;
  std::condition_variable cv_not_full_;
  std::condition_variable cv_not_empty_;

 public:
  explicit BoundedBlockingQueue(int capacity) : capacity_(capacity) {}

  ~BoundedBlockingQueue() {}

  BoundedBlockingQueue(const BoundedBlockingQueue& other) = delete;
  BoundedBlockingQueue operator=(const BoundedBlockingQueue& other) = delete;

  void enqueue(int element) {
    std::unique_lock<std::mutex> lock(mtx_);
    cv_not_full_.wait(
        lock, [this] { return static_cast<int>(queue_.size()) < capacity_; });

    queue_.push(element);
    cv_not_empty_.notify_one();
  }

  int dequeue() {
    std::unique_lock<std::mutex> lock(mtx_);
    cv_not_empty_.wait(lock, [this] { return !queue_.empty(); });

    int val = queue_.front();
    queue_.pop();

    cv_not_full_.notify_one();
    return val;
  }

  int size() {
    std::lock_guard<std::mutex> lock(mtx_);
    return static_cast<int>(queue_.size());
  }
};
