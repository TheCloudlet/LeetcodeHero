// Leetcode 210. Course Schedule II
// @tag: topological-sort, bfs, graph
// @difficulty: medium

#include <queue>
#include <vector>

class Solution {
 public:
  std::vector<int> findOrder(
      int numCourses, const std::vector<std::vector<int>>& prerequisites) {
    // Use vector<vector<int>> not vector<list<int>> for cache locality
    std::vector<std::vector<int>> adj_vec(numCourses);
    std::vector<int> indeg(numCourses, 0);

    for (const auto& pair : prerequisites) {
      const int to = pair.at(0);
      const int from = pair.at(1);
      adj_vec[from].push_back(to);
      ++indeg[to];
    }

    std::queue<int> ready_queue;

    // Find all courses with zero in-degree
    for (int course = 0; course < numCourses; ++course) {
      if (indeg[course] == 0) {
        ready_queue.push(course);
      }
    }

    std::vector<int> result;
    result.reserve(numCourses);

    // Remove courses with zero in-degree and update neighbors
    while (!ready_queue.empty()) {
      int from = ready_queue.front();
      ready_queue.pop();
      result.push_back(from);

      for (const auto& to : adj_vec[from]) {
        if (--indeg[to] == 0) {
          ready_queue.push(to);
        }
      }
    }

    // There is a cycle
    if (static_cast<int>(result.size()) < numCourses) {
      return std::vector<int>{};
    }

    return result;
  }
};
