// Leetcode 1631 Path With Minimum Effor

#include <cassert>
#include <cmath>
#include <numeric>
#include <queue>
#include <vector>

class DSU {
 public:
  int num_components;
  std::vector<int> parent;
  std::vector<int> size;

  explicit DSU(int n) : num_components(n), size(n, 1) {
    parent.resize(n);
    std::iota(parent.begin(), parent.end(), 0);
  }

  int find(const int a) {
    if (parent[a] == a) return a;
    return parent[a] = find(parent[a]);
  }

  bool unite(const int a, const int b) {
    int root_a = find(a);
    int root_b = find(b);

    if (root_a == root_b) return false;

    if (size[root_a] > size[root_b]) std::swap(root_a, root_b);

    parent[root_a] = root_b;
    size[root_b] += size[root_a];
    return true;
  }
};

class Solution {
 public:
  int minimumEffortPath(const std::vector<std::vector<int>>& heights) {
    int num_rows = static_cast<int>(heights.size());
    int num_cols = static_cast<int>(heights[0].size());

    if (num_rows == 1 && num_cols == 1) return 0;

    DSU dsu(num_cols * num_rows);

    // Store {w, u, v};
    std::priority_queue<std::tuple<int, int, int>,
                        std::vector<std::tuple<int, int, int>>, std::greater<>>
        min_heap;

    auto getNodeIndex = [num_cols](const int row, const int col) -> int {
      return row * num_cols + col;
    };

    // Convert Grid to edges
    for (int row = 0; row < num_rows; ++row) {
      for (int col = 0; col < num_cols; ++col) {
        if (col + 1 < num_cols) {
          min_heap.emplace(std::abs(heights[row][col] - heights[row][col + 1]),
                           getNodeIndex(row, col), getNodeIndex(row, col + 1));
        }
        if (row + 1 < num_rows) {
          min_heap.emplace(std::abs(heights[row][col] - heights[row + 1][col]),
                           getNodeIndex(row, col), getNodeIndex(row + 1, col));
        }
      }
    }

    while (!min_heap.empty()) {
      auto [w, u, v] = min_heap.top();
      min_heap.pop();

      (void)dsu.unite(u, v);
      if (dsu.find(0) == dsu.find(num_cols * num_rows - 1)) {
        return w;
      }
    }

    assert(0);
    return -1;
  }
};
