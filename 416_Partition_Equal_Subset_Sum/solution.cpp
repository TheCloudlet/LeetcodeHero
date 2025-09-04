// Leetcode 416. Partition Equal Subset Sum
// @tag: dp, knapsack, neetcode150
// @difficulty: medium

// Soluiton 1: Dynamic Programming with HashSet
// The most easiest to implemeent and understand
// We use a hashset to keep track of all possible sums we can form with the
// elements seen so far. For each number, we iterate through the current sums
// and add the number to each sum to form new sums. If at any point we form
// the target sum (which is half of the total sum), we return true.
// If we finish processing all numbers without finding the target sum, we return
// false.
//
// Time Complexity: O(n * targetSum)
// Space Complexity: O(targetSum)

#if def(SOLUTION_1)

#include <numeric>
#include <unordered_set>
#include <vector>

class Solution {
public:
  bool canPartition(const std::vector<int> &nums) {
    int sum = std::accumulate(nums.begin(), nums.end(), 0);
    if (sum % 2 != 0) {
      return false; // Odd, cannot partition
    }

    int targetSum = sum / 2;
    // Base case: the empty set can always form a sum of 0.
    std::unordered_set<int> possibleSums = {0};

    for (const auto &num : nums) {
      // Create a temporary vector to hold new sums to avoid modifying
      // possibleSums while iterating over it.
      std::vector<int> toAdd;
      for (const auto &sum : possibleSums) {
        int newSum = sum + num;
        if (newSum == targetSum) {
          return true;
        }
        if (newSum < targetSum) {
          toAdd.emplace_back(newSum);
        }
      }
      // Updade with new sums
      for (const auto &newSum : toAdd) {
        possibleSums.insert(newSum);
      }
    }

    return false;
  }
};

#endif