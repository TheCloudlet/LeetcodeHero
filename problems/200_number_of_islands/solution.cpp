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
#include <vector>

class Solution {
 public:
  int numIslands(std::vector<std::vector<char>>& grid) {
    int island_count = 0;
    int n_rows = static_cast<int>(grid.size());
    int n_cols = static_cast<int>(grid[0].size());
    constexpr int kDirs[] = {1, 0, -1, 0, 1};

    auto NukeIsland = [&](auto self, const int row, const int col) -> void {
      if (row < 0 || col < 0 || row >= n_rows || col >= n_cols) return;
      if (grid[row][col] == '0') return;

      grid[row][col] = '0';

      for (int dir = 0; dir < 4; ++dir) {
        const int next_row = row + kDirs[dir];
        const int next_col = col + kDirs[dir + 1];
        self(self, next_row, next_col);
      }
    };

    for (int row = 0; row < n_rows; ++row) {
      for (int col = 0; col < n_cols; ++col) {
        if (grid[row][col] == '1') {
          NukeIsland(NukeIsland, row, col);
          ++island_count;
        }
      }
    }

    return island_count;
  }
};
#endif
