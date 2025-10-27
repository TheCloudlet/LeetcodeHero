// Leetcode 416. Partition Equal Subset Sum
// @tag: dp, knapsack, neetcode150, need-review
// @difficulty: medium

// Soluiton 1: Dynamic Programming with HashSet
//
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

#if defined(SOLUTION_1)

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

// Solution 2: Dynamic Programming with 2D Array
//
// This approach is to treat the problem as a 0/1 knapsack problem.
// The most difficult part is to define the dp state.
//
// Step 1: Define dp state
//   canForm[idx][currSum] = true if we can form the currSum using the first idx
//   of numbers, otherwise false
//
// Step 2: Base case
//   canForm[0][_] = true, we can always form the sum 0
//
// Step 3: State transition
//   canForm[idx][currSum] = canForm[i-1][j] || canForm[i-1][j-nums[i-1]]
//   (beaware of the `j - nums[i-1] >= 0`)
// Step 4: Result
//   canForm[n][targetSum]
//
// Time Complexity: O(n * targetSum)
// Space Complexity: O(n * targetSum)

#if defined(SOLUTION_2)
#include <numeric>
#include <vector>

class Solution {
public:
  bool canPartition(const std::vector<int> &Nums) {
    int totalSum = std::accumulate(Nums.begin(), Nums.end(), 0);
    if (totalSum % 2 != 0) {
      return false; // Odd, cannot partition
    }

    const int targetSum = totalSum / 2;
    const size_t numCount = Nums.size();

    std::vector<std::vector<bool>> canForm(
        numCount + 1, std::vector<bool>(targetSum + 1, false));

    // Base case: A sum of 0 is always possible (by choosing no numbers).
    for (size_t i = 0; i <= numCount; ++i) {
      canForm[i][0] = true;
    }

    // Fill the DP table.
    for (size_t numIdx = 1; numIdx <= numCount; ++numIdx) {
      for (int currentSum = 1; currentSum <= targetSum; ++currentSum) {
        int currentNum = Nums[numIdx - 1];

        // Case 1: We don't use the current number.
        // The possibility is the same as with the previous set of numbers.
        bool possibleWithoutNum = canForm[numIdx - 1][currentSum];

        // Case 2: We use the current number (if the sum allows for it).
        bool possibleWithNum = false;
        if (currentSum >= currentNum) {
          possibleWithNum = canForm[numIdx - 1][currentSum - currentNum];
        }

        canForm[numIdx][currentSum] = possibleWithoutNum || possibleWithNum;
      }
    }

    return canForm[numCount][targetSum];
  }
};
#endif

// Solution 3: Dynamic Programming with 1D Array (Space Optimized)
//
// The trick is to update the dp array in reverse order, so that we do not
// overwrite the results from the current iteration.
//
// Time Complexity: O(n * targetSum)
// Space Complexity: O(targetSum)

#if defined(SOLUTION_3)
#include <numeric>
#include <vector>

class Solution {
public:
  bool canPartition(const std::vector<int> &nums) {
    int sum = std::accumulate(nums.begin(), nums.end(), 0);
    if (sum % 2 != 0) {
      return false; // Odd, cannot partition
    }

    const int targetSum = sum / 2;
    std::vector<bool> canForm(targetSum + 1, false);
    canForm[0] = true;

    for (const int num : nums) {
      for (int currSum = targetSum; currSum >= num; --currSum) {
        // A sum is possible if it was already possible, OR if we can form
        // (currSum - num) and add the current number to it.
        canForm[currSum] = canForm[currSum] || canForm[currSum - num];
      }
    }

    return canForm[targetSum];
  }
};
#endif