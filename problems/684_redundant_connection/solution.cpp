// Leetcode 684. Redundant Connection

#include <numeric>
#include <vector>

class Solution {
  struct DSU {
    std::vector<int> parent;
    std::vector<int> size;
    int component_count;

    // According to the description, we are 1-indexed
    explicit DSU(int n) : parent(n + 1), size(n + 1, 1), component_count(n) {
      std::iota(parent.begin(), parent.end(), 0);
    }

    int find(int x) {
      if (parent[x] == x) return x;
      return parent[x] = find(parent[x]);
    }

    bool unite(int x, int y) {
      int root_x = find(x);
      int root_y = find(y);

      if (root_x == root_y) return false;

      // To avoid the tree becomes linked list
      if (size[root_x] < size[root_y]) {
        std::swap(root_x, root_y);
      }
      parent[root_y] = root_x;
      size[root_x] += size[root_y];
      --component_count;

      return true;
    }
  };

 public:
  std::vector<int> findRedundantConnection(
      const std::vector<std::vector<int>>& edges) {
    DSU dsu(static_cast<int>(edges.size()));

    for (const auto& edge : edges) {
      if (!dsu.unite(edge[0], edge[1])) {
        return edge;
      }
    }

    // For cpp20
    // auto it = std::ranges::find_if(edges, [&dsu](const auto& edge) {
    //   return !dsu.unite(edge[0], edge[1]);
    // });

    // if (it != edges.end()) {
    //     return *it;
    // }

    return {};
  }
};
