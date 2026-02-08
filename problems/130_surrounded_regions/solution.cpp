// Leetcode 130. Surrounded Regions
//
// NOTE:
// Insead of finding the surrounded regions, we find the non-surrounded region
//
// Time Complexity: O(M * N)
// Space Complexity: O(1)
//   where M is nums of rows
//         N is nums of cols

#include <cassert>
#include <vector>

class Solution {
 public:
  void solve(std::vector<std::vector<char>>& board) {
    int n_rows = static_cast<int>(board.size());
    int n_cols = static_cast<int>(board[0].size());

    const std::vector<int> dirs = {-1, 0, 1, 0, -1};

    // If not surrounded, we change to "N"
    const auto dfs = [&](auto self, int r, int c) -> void {  // Add const here
      if (r < 0 || r >= n_rows || c < 0 || c >= n_cols) {
        // Out ouf bound check
        return;
      }

      if (board[r][c] == 'X' || board[r][c] == 'N') {
        // Non process need or processed
        return;
      }

      if (board[r][c] == 'O') {
        board[r][c] = 'N';
        for (int i = 0; i < 4; ++i) {
          self(self, r + dirs[i], c + dirs[i + 1]);
        }
        return;
      }
    };

    // Color not surrounded "O" into "N"
    for (int c = 0; c < n_cols; ++c) {
      dfs(dfs, 0, c);
      dfs(dfs, n_rows - 1, c);
    }

    for (int r = 1; r < n_rows - 1; ++r) {
      dfs(dfs, r, 0);
      dfs(dfs, r, n_cols - 1);
    }

    // Set all char to correct
    for (int r = 0; r < n_rows; ++r) {
      for (int c = 0; c < n_cols; ++c) {
        switch (board[r][c]) {
          case 'O':
            board[r][c] = 'X';
            break;
          case 'X':
            break;
          case 'N':
            board[r][c] = 'O';
            break;
          default:
            assert(0);
        }
      }
    }

    return;
  }
};
