#include <queue>
#include <utility>
#include <vector>

class Solution {
 public:
  int orangesRotting(std::vector<std::vector<int>>& grid) {
    if (grid.empty()) {
      return -1;
    }

    const int rows = static_cast<int>(grid.size());
    const int cols = static_cast<int>(grid[0].size());

    // Group 1: Initialization
    std::queue<std::pair<int, int>> bfs_queue;
    int fresh_count = 0;

    for (int row = 0; row < rows; ++row) {
      for (int col = 0; col < cols; ++col) {
        if (grid[row][col] == 1) {
          ++fresh_count;
        } else if (grid[row][col] == 2) {
          bfs_queue.emplace(row, col);
        }
      }
    }

    static constexpr int kDirs[4][2] = {{1, 0}, {-1, 0}, {0, 1}, {0, -1}};

    int minutes_elapsed = 0; // Notes: for google way, usually adding a new line
                             // above this because this mutable variable not
                             // related to kDir

    while (!bfs_queue.empty() && fresh_count > 0) {
      const int level_size = bfs_queue.size();

      for (int i = 0; i < level_size; ++i) {
        const auto [curr_row, curr_col] = bfs_queue.front();
        bfs_queue.pop();

        for (const auto& [dr, dc] : kDirs) {
          const int next_row = curr_row + dr;
          const int next_col = curr_col + dc;
          if (next_row >= 0 && next_row < rows && next_col >= 0
              && next_col < cols && grid[next_row][next_col] == 1) {
            grid[next_row][next_col] = 2;
            --fresh_count;
            bfs_queue.emplace(next_row, next_col);
          }
        }
      }

      ++minutes_elapsed;
    }

    return fresh_count == 0 ? minutes_elapsed : -1;
  }
};
