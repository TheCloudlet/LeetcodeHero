// Leetcode 74. Search a 2D Matrix

#include <vector>

class Solution {
 public:
  bool searchMatrix(const std::vector<std::vector<int>>& matrix, int target) {
    if (matrix.empty()) return false;

    int num_rows = static_cast<int>(matrix.size());
    int num_cols = static_cast<int>(matrix[0].size());

    int left = 0;
    int right = num_rows * num_cols - 1;

    auto GetRowCol = [&](const int index) -> std::pair<int, int> {
      return {index / num_cols, index % num_cols};
    };

    while (left <= right) {
      const int mid = left + (right - left) / 2;
      const auto [row, col] = GetRowCol(mid);
      if (matrix[row][col] == target)
        return true;
      else if (matrix[row][col] < target)
        left = mid + 1;
      else
        right = mid - 1;
    }

    return false;
  }
};
