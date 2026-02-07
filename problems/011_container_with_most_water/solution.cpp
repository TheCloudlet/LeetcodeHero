// Leetcode 11. Container With Most Water

#include <vector>

class Solution {
 public:
  int maxArea(const std::vector<int>& height) {
    int max_area = 0;
    int left = 0;
    int right = height.size() - 1;

    while (left < right) {
      const int h = std::min(height[left], height[right]);
      max_area = std::max(max_area, h * (right - left));

      if (height[left] < height[right]) {
        ++left;
      } else {
        --right;
      }
    }
    return max_area;
  }
};
