// Leetcode 130. Surrounded Regions
//
// NOTE:
// Insead of finding the surrounded regions, we find the non-surrounded region
//
// Time Complexity: O(M * N)
// Space Complexity: O(1)
//   where M is nums of rows
//         N is nums of cols
//
// Interview:
// If asked what might have an issue, I would point out that using recursive DFS
// is not safe for production due to potential Call Stack Overflow.
//
// Your Answer:
// By using std::queue for BFS or std::stack for Iterative DFS, we shift the
// memory burden from the limited Thread Stack to the much larger Heap memory.

#include <array>
#include <vector>

class Solution {
 public:
  void solve(std::vector<std::vector<char>>& board) {
    if (board.empty() || board[0].empty()) return;

    int num_rows = static_cast<int>(board.size());
    int num_cols = static_cast<int>(board[0].size());

    constexpr std::array<int, 5> kDirs = {1, 0, -1, 0, 1};

    // Add & in self to avoid copy
    auto traverse = [&](auto& self, int r, int c) -> void {
      if (r < 0 || c < 0 || r >= num_rows || c >= num_cols) return;

      if (board[r][c] != 'O') return;

      board[r][c] = 'R';

      for (int dir = 0; dir < 4; ++dir) {
        self(self, r + kDirs[dir], c + kDirs[dir + 1]);
      }
    };

    for (int c = 0; c < num_cols; ++c) {
      traverse(traverse, 0, c);             // Top
      traverse(traverse, num_rows - 1, c);  // Bottom
    }
    for (int r = 1; r < num_rows - 1; ++r) {
      traverse(traverse, r, 0);             // Left
      traverse(traverse, r, num_cols - 1);  // Right
    }

    for (int row = 0; row < num_rows; ++row) {
      for (int col = 0; col < num_cols; ++col) {
        if (board[row][col] == 'O') {
          board[row][col] = 'X';
        } else if (board[row][col] == 'R') {
          board[row][col] = 'O';
        }
      }
    }
  }
};
