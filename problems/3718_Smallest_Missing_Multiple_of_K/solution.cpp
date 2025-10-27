// Leetcode 3718. Smallest Missing Multiple of K
// @tags: math, hashtable, iteration, leetcode-contest
// @difficulty: easy
// @contest-spent-time: 8 min 23 sec

#include <algorithm>
#include <limits>
#include <unordered_set>
#include <vector>

class Solution {
public:
  int missingMultiple(vector<int> &nums, int k) {
    std::unordered_set<int> numSet;
    int maxNum = std::numeric_limits<int>::min();
    for (const auto &num : nums) {
      maxNum = std::max(maxNum, num);
      numSet.insert(num);
    }

    // Check all multiples of k up to maxNum
    for (int coeff = 1; coeff * k <= maxNum; ++coeff) {
      int multiple = coeff * k;
      if (numSet.find(multiple) == numSet.end()) {
        return multiple;
      }
    }

    // All multiples up to maxNum exist, return the next one
    return maxNum + k - (maxNum % k);
  }
};