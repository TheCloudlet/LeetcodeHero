// Task:
// Implement a TaskScheduler that manages recurring tasks.
//
// schedule(interval, fn):
//   Registers a callable fn to run after `interval` milliseconds.
//   - If fn returns true, remove it permanently.
//   - If fn returns false, reschedule for now + interval.

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

  std::priority_queue<Task, std::vector<Task>, std::greater<Task>> task_queue;

 public:
  TaskScheduler() = default;
  ~TaskScheduler() = default;

  TaskScheduler(const TaskScheduler&) = delete;
  TaskScheduler& operator=(const TaskScheduler&) = delete;
  TaskScheduler(TaskScheduler&&) = delete;
  TaskScheduler& operator=(TaskScheduler&&) = delete;

  bool schedule(Interval interval, std::function<bool()> fp) {
    auto first_run_time = std::chrono::steady_clock::now() + interval;
    task_queue.emplace(Task{first_run_time, interval, fp});
    return true;
  }

  void worker_loop() {
    while (!task_queue.empty()) {
      Task task = task_queue.top();
      auto now = std::chrono::steady_clock::now();

      if (now < task.time_point) {
        std::this_thread::sleep_until(task.time_point);
      }

      task_queue.pop();

      bool ret = task.fp();
      if (ret == false) {
        task.time_point += task.interval;
        task_queue.push(std::move(task));
      }
    }
  }
};
#endif
