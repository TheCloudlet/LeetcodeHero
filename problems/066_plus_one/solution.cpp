// Leetcode 66. Plus One
// @tag: array, math
// @difficulty: easy

class Solution {
 public:
  std::vector<int> plusOne(std::vector<int>& digits) {
    std::vector<int> result = digits;

    for (int idx = result.size() - 1; idx >= 0; --idx) {
      if (result[idx] < 9) {
        result[idx]++;
        return result; // Early exit, very efficient for [1, 2, 3]
      }
      result[idx] = 0;
    }

    // If we finished the loop, it means ALL digits were 9.
    result.insert(result.begin(), 1);
    return result;
  }
};