// Leetcode 80. Remove Duplicates from Sorted Array II

// Time Complexity: O(N)
// Space Complexity: O(1)
//   where N is the size of nums

#if defined(my)
#include <vector>

class Solution {
 public:
  int removeDuplicates(std::vector<int>& nums) {
    if (nums.empty()) {
      return 0;
    }

    size_t slow = 0; // The prev insertion position
    int dup_count = 0;

    for (size_t fast = 1; fast < nums.size(); ++fast) {
      if (nums[fast] == nums[slow] && dup_count < 1) {
        ++slow;
        nums[slow] = nums[fast];
        ++dup_count;
      } else if (nums[fast] != nums[slow]) {
        ++slow;
        nums[slow] = nums[fast];
        dup_count = 0;
      }
    }

    nums.resize(slow + 1);

    return slow + 1;
  }
};
#endif

#if defined(improved)
#include <vector>

class Solution {
 public:
  int removeDuplicates(std::vector<int>& nums) {
    if (static_cast<int>(nums.size()) < 2) {
      return static_cast<int>(nums.size());
    }

    size_t slow = 2;  // Index to the next insertion point

    for (size_t fast = 2; fast < nums.size(); ++fast) {
      if (nums[fast] != nums[slow - 2]) {
        nums[slow] = nums[fast];
        ++slow;
      }
    }

    nums.resize(slow);

    return slow;
  }
};
#endif
