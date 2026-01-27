// Leetcode 26: Remove Duplicates from Sorted Array

// Note:
// Initially attempted using erase() backwards (influenced by LISP/Racket),
// but the two-pointer approach is the correct O(N) solution.

// Time Complexity: O(N)
// Space Complexity: O(1)
//   where N is the size of the input array

#include <vector>

class Solution {
 public:
  int removeDuplicates(std::vector<int>& nums) {
    if (nums.empty()) {
      return 0;
    }

    int slow = 0;

    for (int fast = 1; fast < nums.size(); ++fast) {
      // Found a new unique element
      if (nums[fast] != nums[slow]) {
        ++slow;
        nums[slow] = nums[fast];
      }
    }

    // But the better solution is to use nums.resize(slow + 1);
    nums.erase(nums.begin() + slow + 1, nums.end());

    return slow + 1;
  }
};
