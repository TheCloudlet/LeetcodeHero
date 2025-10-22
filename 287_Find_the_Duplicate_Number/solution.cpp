// 287. Find the Duplicate Number
// @tag array, binary-search, linked-list, two-pointer, counting
// @difficulty medium

#if defined(VECOTOR)
// Time Complexity: O(n)
// Space Complexity: O(n)

#include <vector>

class Solution {
 public:
  int findDuplicate(const std::vector<int>& nums) {
    std::vector<bool> unique_num(nums.size(), false);
    for (const int& num : nums) {
      if (unique_num[num]) {
        return num;
      } else {
        unique_num[num] = true;
      }
    }
    return -1;
  }
};
#endif

#if defined(HASH_MAP)
// Time Complexity: O(n)
// Space Complexity: O(n)

#include <unordered_set>
#include <vector>

class Solution {
 public:
  int findDuplicate(const std::vector<int>& nums) {
    std::unordered_set<int> unique_num;
    for (const int& num : nums) {
      if (unique_num.find(num) != unique_num.end()) {
        return num;
      } else {
        unique_num.insert(num);
      }
    }
    return -1;
  }
};

#endif

// TODO: Review the Floyd's Tortoise and Hare (Cycle Detection) algorithm.
#if defined(TWO_POINTER)

// Time Complexity: O(n)
// Space Complexity: O(1)
//
// This is very not intuitive solution. The idea is to treat the array as a
// linked list. Each index is a node, and the value at that index is the next
// node's index. Since there is a duplicate number, there must be a cycle in
// the linked list. We can use Floyd's Tortoise and Hare algorithm to detect
// the cycle and find the entrance to the cycle, which is the duplicate number.

#include <vector>
class Solution {
 public:
  int findDuplicate(std::vector<int>& nums) {
    int fast = 0, slow = 0;

    do {
      slow = nums[slow];
      fast = nums[nums[fast]];
    } while (slow != fast);

    int slow_2 = 0;
    while (slow != slow_2) {
      slow = nums[slow];
      slow_2 = nums[slow_2];
    }

    return slow;
  }
};

#endif