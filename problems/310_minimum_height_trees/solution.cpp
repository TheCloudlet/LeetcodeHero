// Leetcode 310. Minimum Height Trees
//
// Thinking:
// Prune graph node from outer to inner

#include <vector>

class Solution {
public:
  std::vector<int> findMinHeightTrees(int n,
                                      std::vector<std::vector<int>> &edges) {
    if (1 == n) {
      return {0}; // edge case
    }

    std::vector<std::vector<int>> adjList(n);
    std::vector<int> degree(n);
    for (const auto &e : edges) {
      adjList[e.at(0)].emplace_back(e.at(1));
      adjList[e.at(1)].emplace_back(e.at(0));
      ++degree[e.at(0)];
      ++degree[e.at(1)];
    }

    std::vector<int> leaves;
    for (int i = 0; i < n; ++i) {
      if (degree[i] == 1) {
        leaves.emplace_back(i);
      }
    }
    // Last one or two nodes left after pruning are the centroids of the tree
    while (n > 2) {
      std::vector<int> newLeaves;
      for (const int &u : leaves) {
        --n;
        for (const auto &neighbor : adjList[u]) {
          if (1 == --degree[neighbor]) {
            newLeaves.emplace_back(neighbor);
          }
        }
      }
      leaves = std::move(newLeaves);
    }
    return leaves;
  }
};

