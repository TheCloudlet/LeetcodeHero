// Task:
// Implement a TaskScheduler that manages recurring tasks.
//
// schedule(interval, fn):
//   Registers a callable fn to run after `interval` milliseconds.
//   - If fn returns true,  reschedule for now + interval.
//   - If fn returns false, remove it permanently.

#if defined(SINGLE_THREAD)
#include <algorithm>
#include <chrono>
#include <functional>
#include <queue>
#include <thread>

class TaskScheduler {
 private:
  using TimePoint = std::chrono::steady_clock::time_point;
  using Interval = std::chrono::milliseconds;

  struct Task {
    TimePoint time_point;
    Interval interval;
    std::function<bool()> fp;

    bool operator>(const Task& other) const {
      return time_point > other.time_point;
    }
  };

  std::priority_queue<Task, std::vector<Task>, std::greater<Task>> task_queue_;

 public:
  TaskScheduler() = default;
  ~TaskScheduler() = default;

  TaskScheduler(const TaskScheduler&) = delete;
  TaskScheduler& operator=(const TaskScheduler&) = delete;
  TaskScheduler(TaskScheduler&&) = delete;
  TaskScheduler& operator=(TaskScheduler&&) = delete;

  bool schedule(Interval interval, std::function<bool()> fp) {
    auto first_run_time = std::chrono::steady_clock::now() + interval;
    task_queue_.emplace(Task{first_run_time, interval, fp});
    return true;
  }

  void worker_loop() {
    while (!task_queue_.empty()) {
      Task task = task_queue_.top();
      auto now = std::chrono::steady_clock::now();

      if (now < task.time_point) {
        std::this_thread::sleep_until(task.time_point);
      }

      task_queue_.pop();

      bool ret = task.fp();
      if (ret == false) {
        task.time_point += task.interval;
        task_queue_.push(std::move(task));
      }
    }
  }
};
#endif

#if defined(MULTI_THREAD)
#include <algorithm>
#include <chrono>
#include <functional>
#include <queue>
#include <thread>

class TaskScheduler {
 private:
  using TimePoint = std::chrono::steady_clock::time_point;
  using Interval = std::chrono::milliseconds;

  struct Task {
    TimePoint time_point;
    Interval interval;
    std::function<bool()> fp;

    bool operator>(const Task& other) const {
      return time_point > other.time_point;
    }
  };

  std::priority_queue<Task, std::vector<Task>, std::greater<Task>> task_queue_;

  // Multi-thread control
  std::mutex mtx_;
  std::condition_variable cv_;
  bool stop_flag_ = false;

 public:
  TaskScheduler() = default;
  ~TaskScheduler() {
    {
      std::lock_guard<std::mutex> lock(mtx_);
      stop_flag_ = true;
    }
    cv_.notify_all();
    return;
  };

  TaskScheduler(const TaskScheduler&) = delete;
  TaskScheduler& operator=(const TaskScheduler&) = delete;
  TaskScheduler(TaskScheduler&&) = delete;
  TaskScheduler& operator=(TaskScheduler&&) = delete;

  bool schedule(Interval interval, std::function<bool()> fp) {
    auto first_run_time = std::chrono::steady_clock::now() + interval;
    {  // Critical Sessioj
      std::lock_guard<std::mutex> lock(mtx_);
      task_queue_.emplace(Task{first_run_time, interval, fp});
    }

    cv_.notify_one();
    return true;
  }

  void worker_loop() {
    Task task;

    while (true) {
      {
        std::unique_lock<std::mutex> lock(mtx_);

        cv_.wait(lock, [this] { return !task_queue_.empty() || stop_flag_; });

        if (task_queue_.empty() || stop_flag_ == true) return;

        task = task_queue_.top();
        auto now = std::chrono::steady_clock::now();
        auto top_start_time = task.time_point;

        if (now < top_start_time) {
          cv_.wait_until(lock, task.time_point);
          continue;
        }

        task_queue_.pop();
      }

      bool ret = task.fp();
      if (ret == false) {
        task.time_point += task.interval;
        {
          std::lock_guard<std::mutex> lock(mtx_);
          task_queue_.push(std::move(task));
        }

        cv_.notify_one();
      }
    }
  }
};
#endif
