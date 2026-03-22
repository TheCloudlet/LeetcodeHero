// Leetcode 268. Missing Number
//
// Solution: Use XOR property.
//   - XOR of all numbers from 0 to n gives a value.
//   - XOR with all elements of the array cancels out the numbers that appear,
//     leaving the missing number.
//   This works because a ^ a = 0 and XOR is commutative and associative.
#include <vector>

class Solution {
 public:
  int missingNumber(const std::vector<int>& nums) {
    int n = static_cast<int>(nums.size());
    int xor_sum = 0;

    for (int i = 0; i <= n; ++i) {
      xor_sum ^= i;
    }

    for (const int num : nums) {
      xor_sum ^= num;
    }

    return xor_sum;
  }
};
