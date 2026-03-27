// Leetcode 207. Course Schedule
// @tags: graph, dfs, bfs, topological_sort, neetcode150
// @difficulty: medium

#include <vector>

class Solution {
 public:
  // 定義強型別的狀態機，取代魔法數字
  enum class State { kUnvisited, kVisiting, kVisited };

  bool canFinish(int numCourses,
                 const std::vector<std::vector<int>>& prerequisites) {
    // Create adjacent list
    std::vector<std::vector<int>> adj_list(numCourses);
    for (const auto& prereq : prerequisites) {
      int to = prereq[0];
      int from = prereq[1];
      adj_list[from].push_back(to);
    }

    std::vector<State> state(numCourses, State::kUnvisited);

    auto has_cycle = [&](auto self, int from) -> bool {
      if (state[from] == State::kVisiting) return true;
      if (state[from] == State::kVisited) return false;

      state[from] = State::kVisiting;

      // int 取代 const auto&
      for (int to : adj_list[from]) {
        if (self(self, to)) return true;
      }

      state[from] = State::kVisited;
      return false;
    };

    for (int i = 0; i < numCourses; ++i) {
      if (state[i] == State::kUnvisited && has_cycle(has_cycle, i)) {
        return false;
      }
    }

    return true;
  }
};
