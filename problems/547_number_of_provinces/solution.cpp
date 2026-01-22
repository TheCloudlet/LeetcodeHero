// Leecode 547. Number of Provinces

// Time Complexity: O(N^2)
//   We iterate through the upper triangle of the N*N adjacency matrix,
//   performing approximately (N^2)/2 checks. Inside the loop, the DSU
//   operations (union/find) take Amortized O(alpha(N)) ~ O(1) time.
//   Total time = O(N^2 * alpha(N)) ≈ O(N^2).

// Space Complexity: O(N)
//   where N is the number of cities (nodes).
//   We need O(N) auxiliary space for the DSU 'parent' and 'size' vectors.
//   The recursion stack for 'find' is negligible (O(alpha(N))) due to path compression.

#include <cassert>
#include <numeric>
#include <vector>

class Solution {
 public:
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

  int findCircleNum(const std::vector<std::vector<int>>& isConnected) {
    if (isConnected.empty()) {
      return 0;
    }

    size_t n = isConnected.size();
    size_t m = isConnected[0].size();
    assert(n == m);

    DSU dsu(n);

    for (size_t row = 0; row < n; ++row) {
      for (size_t col = row + 1; col < n; ++col) {
        if (isConnected[row][col] == 1) {
          dsu.unite(row, col);
        }
      }
    }

    return dsu.getNumComponents();
  }
};
