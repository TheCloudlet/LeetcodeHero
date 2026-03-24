// Leetcode 3613. Minimize Maximum Component Cost
//
// NOTE: real interview
//
// Time Complexity: O(E log E), due to sorting
// Space Complexity: O(N);

#include <algorithm>
#include <numeric>
#include <vector>

class DSU {
 public:
  std::vector<int> parents;
  int components;

  explicit DSU(int n) : components(n) {
    parents.resize(n);
    std::iota(parents.begin(), parents.end(), 0);
  }

  int find(int i) {
    if (parents[i] == i) {
      return i;
    }
    return parents[i] = find(parents[i]);
  }

  void unite(int i, int j) {
    int root_i = find(i);
    int root_j = find(j);
    if (root_i != root_j) {
      parents[root_j] = root_i;
      --components;
    }
  }
};

class Solution {
 public:
  int minCost(int n, std::vector<std::vector<int>>& edges, int k) {
    if (k >= n) {
      return 0;
    }

    std::sort(edges.begin(), edges.end(),
              [](const std::vector<int>& a, const std::vector<int>& b) {
                return a[2] < b[2];
              });

    DSU dsu(n);
    int max_weight = 0;

    for (const auto& edge : edges) {
      const int from = edge[0];
      const int to = edge[1];
      const int weight = edge[2];

      if (dsu.find(from) != dsu.find(to)) {
        dsu.unite(from, to);
        max_weight = weight;  // weight is already sorted
        if (dsu.components <= k) break;
      }
    }

    return max_weight;
  }
};
