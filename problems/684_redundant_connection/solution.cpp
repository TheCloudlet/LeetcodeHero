// Leetcode 684. Redundant Connection

#include <numeric>
#include <vector>

class Solution {
  struct DSU {
    std::vector<int> parent;
    std::vector<int> size;
    int num_components;

    explicit DSU(int n) : parent(n), size(n, 1), num_components(n) {
      std::iota(parent.begin(), parent.end(), 0);
    }

    int find(int x) {
      if (parent[x] == x) {
        return x;
      }
      return parent[x] = find(parent[x]);
    }

    bool unite(int x, int y) {
      int root_x = find(x);
      int root_y = find(y);

      if (root_x == root_y) {
        return false;
      }

      if (size[root_x] > size[root_y]) {
        std::swap(root_x, root_y);
      }
      parent[root_x] = root_y;
      size[root_y] += size[root_x];
      --num_components;
      return true;
    }

    int getSize(int x) { return size[find(x)]; }

    int getNumComponents() { return num_components; }
  };

 public:
  std::vector<int> findRedundantConnection(
      std::vector<std::vector<int>>& edges) {
    if (edges.empty()) {
      return {};
    }

    DSU dsu(static_cast<int>(edges.size()));

    for (const auto& edge : edges) {
      if (!dsu.unite(edge[0] - 1, edge[1] - 1)) {  // -1 is because 1-indexed
        return edge;
      }
    }

    return {};  // Should not reach here
  }
};
