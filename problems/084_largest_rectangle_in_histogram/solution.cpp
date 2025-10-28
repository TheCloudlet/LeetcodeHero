// Leetcode 84. Largest Rectangle in Histogram
// @tag: array, stack, monotonic-stack, neetcode150
// @difficulty: hard

// NOTE:
// The key concept is for every height in heights
// the area of the height is using currently hight is $height * width$
//
//   0  1  2  3  4  5 <-- Index
// [ 2, 1, 5, 6, 2, 3] <-- Heights
//
// [-1,-1, 1, 2, 1, 4] <-- left_smaller
// [ 1, 6, 4, 4, 6, 6] <-- right_smaller
//

#include <algorithm>
#include <stack>
#include <vector>

class Solution {
 public:
  int largestRectangleArea(std::vector<int>& heights) {
    std::stack<int> mono_stack;

    // Index of nearest smaller element on each side
    // If none, use heights.size() for right, -1 for left
    std::vector<int> right_smaller(heights.size(), heights.size());
    std::vector<int> left_smaller(heights.size(), -1);

    for (int idx = 0; idx < heights.size(); ++idx) {
      while (!mono_stack.empty() && heights[idx] < heights[mono_stack.top()]) {
        right_smaller[mono_stack.top()] = idx;
        mono_stack.pop();
      }
      mono_stack.push(idx);
    }

    while (!mono_stack.empty()) mono_stack.pop();  // Reset the stack

    for (int idx = heights.size() - 1; idx >= 0; --idx) {
      while (!mono_stack.empty() && heights[idx] < heights[mono_stack.top()]) {
        left_smaller[mono_stack.top()] = idx;
        mono_stack.pop();
      }
      mono_stack.push(idx);
    }

    while (!mono_stack.empty()) mono_stack.pop();

    int max_area = 0;
    for (int idx = 0; idx < heights.size(); ++idx) {
      int right_edge = right_smaller[idx];
      int left_edge = left_smaller[idx];
      // The real boundary is left_edge + 1 and right_edge - 1
      // So the width is $(right_edge - 1) - (left_edge + 1) + 1$
      int width = right_edge - left_edge - 1; 
      max_area = std::max(max_area, heights[idx] * width);
    }

    return max_area;
  }
};