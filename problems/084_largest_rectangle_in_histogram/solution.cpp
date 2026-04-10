// Leetcode 84. Largest Rectangle in Histogram
// @tag: array, stack, monotonic-stack, neetcode150
// @difficulty: hard

#if defined(THREE_PASSES)
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
#endif

#if defined(ONE_PASS)
#include <algorithm>
#include <stack>
#include <vector>

class Solution {
 public:
  int largestRectangleArea(std::vector<int>& heights) {
    // TRICK: Append a dummy bar of height 0 at the end.
    // This acts as a sweeper to forcefully pop and evaluate all remaining
    // elements in the stack when the loop reaches the end.
    heights.push_back(0);

    const int n = static_cast<int>(heights.size());
    int max_area = 0;
    std::stack<int> pending;  // Stores indices, not heights

    for (int i = 0; i < n; ++i) {
      // Maintain a monotonically increasing stack:
      // If we encounter a bar shorter than the top of the stack, it acts as the
      // "Next Smaller" trigger. We must stop and calculate the area for the
      // stack top.
      while (!pending.empty() && heights[i] < heights[pending.top()]) {
        // 1. Determine Height:
        // The bar being popped is the bottleneck height for this specific
        // rectangle.
        const int h = heights[pending.top()];
        pending.pop();

        // 2. Determine Width:
        // Right boundary: The current index 'i' (the Next Smaller element that
        // triggered the pop). Left boundary: The new stack top after popping
        // (the Previous Smaller element). If the stack is empty, no bars to the
        // left are shorter, so the width extends to index 0.
        const int w = pending.empty() ? i : (i - pending.top() - 1);

        max_area = std::max(max_area, h * w);
      }
      pending.push(i);
    }

    return max_area;
  }
};
#endif
