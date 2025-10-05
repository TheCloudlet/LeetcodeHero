// 3702. Longest Subsequence With Non-Zero Bitwise XOR
// @tag: weekly-contest470

#include <vector>

// In real contest, I didn't spot there are only three cases.
// So I tried to use DP to solve it, which is overkill.

class Solution {
public:
  int longestSubsequence(std::vector<int> &nums) {
    int numCount = nums.size();
    int zeroCount = 0;
    int xorAll = 0;

    for (int num : nums) {
      if (num == 0) {
        ++zeroCount;
      }
      xorAll ^= num;
    }

    // 1. All elements are zero.
    if (zeroCount == numCount) {
      return 0;
    }

    // 2. The XOR of all elements is non-zero.
    if (xorAll) {
      return numCount;
    }

    // 3. The XOR of all elements is zero, and there is at least one non-zero
    // element.We can always remove one non-zero element to make the XOR
    // non-zero.
    return numCount - 1;
  }
};