// Leetcode 3712. Sum of Elements With Frequency Divisible by K
// @tag: hashtable, array, leetcode-contest
// @difficulty: easy

#include <unordered_map>
#include <vector>

class Solution {
public:
  int sumDivisibleByK(std::vector<int> &nums, int k) {
    // Count frequency of each element
    std::unordered_map<int, int> frequencyMap;

    for (const auto &num : nums) {
      ++frequencyMap[num];
    }

    // Sum elements whose frequency is divisible by k
    int result = 0;
    for (const auto &[num, count] : frequencyMap) {
      if (count % k == 0) {
        result += (num * count);
      }
    }

    return result;
  }
};
