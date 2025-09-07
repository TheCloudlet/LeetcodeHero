// Leetcode 207. Course Schedule
// @tags: graph, dfs, bfs, topological_sort, neetcode150
// @difficulty: medium

// Solution 1: DFS with coloring
// NOTE:
// 1.  Helper fucntion can be
//     `std::function<bool(int)> dfs = [&](int u) -> bool { ... }`
//     this is a total functional programming style.
#if defined(DFS_COLORING)
#include <vector>
#include <cassert>

class Solution {
public:
  bool canFinish(int numCourses,
                 const std::vector<std::vector<int>> &prerequisites) {
    // Initialize adjacency list with proper size
    std::vector<std::vector<int>> adjVec(numCourses);
    for (const auto &pair : prerequisites) {
      int course = pair[0];
      int prerequisite = pair[1];
      adjVec[prerequisite].push_back(course);
    }

    std::vector<DFSColor> colors(numCourses, DFSColor::unvisited);

    // Check each course for cycles
    for (int node = 0; node < numCourses; ++node) {
      if (colors[node] == DFSColor::unvisited) {
        if (!dfsHelper(adjVec, colors, node)) {
          return false; // Found cycle
        }
      }
    }
    return true;
  }

private:
  enum class DFSColor { unvisited, visiting, visited };

  bool dfsHelper(const std::vector<std::vector<int>> &adjVec,
                 std::vector<DFSColor> &colors, int currNode) {
    colors[currNode] = DFSColor::visiting;

    for (const auto &neighbor : adjVec[currNode]) {
      if (colors[neighbor] == DFSColor::visiting) {
        return false; // Found cycle
      }
      if (colors[neighbor] == DFSColor::unvisited &&
          !dfsHelper(adjVec, colors, neighbor)) {
        return false;
      }
    }

    colors[currNode] = DFSColor::visited;
    return true;
  }
};
#endif

// Solution 2: Khan's Algorithm (BFS with coloring)
#if defined(KHAN_ALGO)
// The soluiton can be found in Course Schedule II (Leetcode 210)
#endif