// Leetcode 1188. Design Bounded Blocking Queue

#if defined(BASIC_QUEUE)
#include <condition_variable>
#include <mutex>
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
#endif

#if defined(RING_BUFF)
#include <condition_variable>
#include <mutex>
#include <thread>
#include <vector>

class BoundedBlockingQueue {
 private:
  std::vector<int> v_;
  int front_;
  int end_;
  int size_;

  // Thread control
  std::mutex mtx_;
  std::condition_variable full_cv_;
  std::condition_variable empty_cv_;

  // Helper
  inline int getNext(int i) const { return (i + 1 == size_) ? 0 : i + 1; }
  inline int getPrev(int i) const { return (i == 0) ? size_ - 1 : i - 1; };

 public:
  explicit BoundedBlockingQueue(int capacity)
      : front_(0), end_(0), size_(capacity + 1) {
    v_.resize(capacity + 1);
  }

  void enqueue(int element) {
    std::unique_lock<std::mutex> lock(mtx_);
    full_cv_.wait(lock, [this] { return getNext(end_) != front_; });

    v_[end_] = element;
    end_ = getNext(end_);

    empty_cv_.notify_one();
  }

  int dequeue() {
    std::unique_lock<std::mutex> lock(mtx_);
    empty_cv_.wait(lock, [this] { return front_ != end_; });

    int val = v_[front_];
    front_ = getNext(front_);

    full_cv_.notify_one();
    return val;
  }

  int size() const {
    std::lock_guard<std::mutex> lock(mtx_);
    return end_ - front_ >= 0 ? end_ - front_ : end_ - front_ + size_;
  }
};
#endif
