// Leetcode 3712. Sum of Elements With Frequency Divisible by K
// @tag: hashtable, array, leetcode-contest
// @difficulty: easy

#include <unordered_map>
#include <vector>

class Solution {
public:
  int sumDivisibleByK(const std::vector<int> &nums, int k) {
    // Count frequency of each element
    std::unordered_map<int, int> frequencyMap;

    for (const auto &num : nums) {
      ++frequencyMap[num];
    }

    // Sum elements whose frequency is divisible by k
    long long result = 0; // NOTE: Use long long to avoid overflow
    for (const auto &[num, count] : frequencyMap) {
      if (count % k == 0) {
        result += const_cast<long long>(num) * count;
      }
    }

    return result;
  }
};
