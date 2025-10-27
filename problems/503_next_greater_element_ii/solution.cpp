// Leetcode 503. Next Greater Element II
//
// NOTE:
// Compared to LC496, this problem is circular.
// Very similar pattern, except we must iterate the array twice.

#include <stack>
#include <vector>

class Solution {
public:
  std::vector<int> nextGreaterElements(std::vector<int> &nums) {

    std::stack<int> monoStack;
    std::vector<int> result(nums.size(), -1);

    for (int i = 0; i < nums.size() * 2; i++) {
      int circularIdx = i % nums.size();
      while (!monoStack.empty() && nums[monoStack.top()] < nums[circularIdx]) {
        result[monoStack.top()] = nums[circularIdx];
        monoStack.pop();
      }
      if (i < nums.size()) { // second round we don't insert to stack
        monoStack.emplace(circularIdx);
      }
    }

    return result;
  }
};
