// Leetcode 2022. Convert 1D Array Into 2D Array

#if defined(SIMPLE)
#include <vector>

class Solution {
 public:
  std::vector<std::vector<int>> construct2DArray(
      const std::vector<int>& original, int m, int n) {
    const int orginal_sz = static_cast<int>(original.size());
    if (m * n != orginal_sz) return {};

    std::vector<std::vector<int>> ans(m, std::vector<int>(n, 0));

    for (int i = 0; i < orginal_sz; ++i) {
      const int col = i % n;
      const int row = i / n;
      ans[row][col] = original[i];
    }

    return ans;
  }
};
#endif

#if defined(BETTER)
#include <vector>

class Solution {
 public:
  std::vector<std::vector<int>> construct2DArray(
      const std::vector<int>& original, int m, int n) {
    const int orginal_sz = static_cast<int>(original.size());
    if (m * n != original_sz) return {};

    std::vector<std::vector<int>> res;
    res.reserve(m);

    for (int i = 0; i < m; ++i) {
      auto start_itr = original.begin() + i * n;
      auto end_itr = start_itr + n;
      res.emplace_back(start_itr, end_itr);
    }

    return res;
  }
};
#endif
