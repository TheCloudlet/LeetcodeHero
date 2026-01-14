// Leetcode 200. Number of Islands

#if defined(my_soultion)
#include <vector>

class Solution {
 public:
  int numIslands(std::vector<std::vector<char>>& grid) {
    if (grid.empty()) {
      return 0;
    }

    int island = 0;

    const int n_rows = static_cast<int>(grid.size());
    const int n_cols = static_cast<int>(grid[0].size());

    for (int row = 0; row < n_rows; ++row) {
      for (int col = 0; col < n_cols; ++col) {
        if (grid[row][col] == '1') {
          ++island;
          nukeIsland(grid, row, col);
        }
      }
    }

    return island;
  }

 private:
  void nukeIsland(std::vector<std::vector<char>>& grid, int row, int col) {
    const int n_rows = static_cast<int>(grid.size());
    const int n_cols = static_cast<int>(grid[0].size());

    if (row < 0 || row >= n_rows || col < 0 || col >= n_cols) {
      return;
    }

    if (grid[row][col] == '0') {
      return;
    }

    grid[row][col] = '0';  // Nuke the land to sea

    constexpr int dirs[4][2] = {{1, 0}, {-1, 0}, {0, 1}, {0, -1}};

    for (const auto& [a, b] : dirs) {
      nukeIsland(grid, a + row, b + col);
    }

    return;
  }
};
#endif

#if defined(modern_cpp_17)
class Solution {
 public:
  int numIslands(std::vector<std::vector<char>>& grid) {
    if (grid.empty()) {
      return 0;
    }

    int island = 0;

    const int n_rows = static_cast<int>(grid.size());
    const int n_cols = static_cast<int>(grid[0].size());

    constexpr int dirs[4][2] = {{1, 0}, {-1, 0}, {0, 1}, {0, -1}};

    auto nukeIsland = [&](auto&& self, int r, int c) -> void {
      if (r < 0 || r >= n_rows || c < 0 || c >= n_cols) {
        return;
      }

      if (grid[r][c] == '0') {
        return;
      }

      grid[r][c] = '0';  // Nuke the land to sea
      for (const auto& [a, b] : dirs) {
        self(self, a + r, b + c);
      }
    };

    for (int row = 0; row < n_rows; ++row) {
      for (int col = 0; col < n_cols; ++col) {
        if (grid[row][col] == '1') {
          ++island;
          nukeIsland(nukeIsland, row, col);
        }
      }
    }

    return island;
  }
};
#endif
