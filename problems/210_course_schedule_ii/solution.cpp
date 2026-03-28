// Leetcode 210. Course Schedule II
// @tag: topological-sort, bfs, graph
// @difficulty: medium

#include <queue>
#include <vector>

class Solution {
 public:
  std::vector<int> findOrder(
      int numCourses, const std::vector<std::vector<int>>& prerequisites) {
    // Use vector<vector<int>> instead of list for better cache locality
    std::vector<std::vector<int>> adj_list(numCourses);
    std::vector<int> in_degrees(numCourses, 0);

    for (const auto& prereq : prerequisites) {
      const int to = prereq[0];
      const int from = prereq[1];
      adj_list[from].push_back(to);
      ++in_degrees[to];
    }

    std::queue<int> ready_queue;

    // Find all courses with zero in-degree (The Initial Frontier)
    for (int course = 0; course < numCourses; ++course) {
      if (in_degrees[course] == 0) {
        ready_queue.push(course);
      }
    }

    std::vector<int> course_order;
    course_order.reserve(numCourses);

    // Process independent courses and update neighbors (Data-flow)
    while (!ready_queue.empty()) {
      const int curr = ready_queue.front();
      ready_queue.pop();
      course_order.push_back(curr);

      for (const int next_course : adj_list[curr]) {
        if (--in_degrees[next_course] == 0) {
          ready_queue.push(next_course);
        }
      }
    }

    // If order size doesn't match numCourses, a cycle exists
    if (static_cast<int>(course_order.size()) != numCourses) {
      return {};
    }

    return course_order;
  }
};
