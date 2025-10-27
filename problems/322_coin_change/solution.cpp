// Leetcode 322. Coin Change
// @tag: dp, bfs, neetcode150, need-review
// @difficulty: medium

// NOTE:
//
// (1) Wrong assumption
// The mistake I made here is to treat this problem as a greedy problem,
// which means always try to use the largest coin first. But in this folloing
// example. [4, 3, 1], amount = 6
// The greedy approach will give 2 (4 + 1 + 1), but the optimal solution is
// 2 (3 + 3).
//
// (2) Make use of std::optional
// This is a good way to represent a value that might be absent.
//
// (3) Memoization value
// Use -1 to represent not computed yet, INT_MAX to represent impossible.

#include <algorithm>
#include <optional>
#include <vector>

class Solution {
public:
  int coinChange(const std::vector<int> &coins, const int amount) {
    int n = coins.size();
    if (n == 0) {
      return -1;
    }
    std::vector<int> sortedCoins = coins;
    std::sort(sortedCoins.begin(), sortedCoins.end(), std::greater<>());
    std::vector<int> memo(amount + 1, -1);
    memo[0] = 0; // base case

    auto res = findSmallestChange(sortedCoins, amount, memo);
    if (res.has_value()) {
      return res.value();
    }

    return -1;
  }

private:
  std::optional<int> findSmallestChange(std::vector<int> &coins, int amount,
                                        std::vector<int> &memo) {
    if (amount < 0) {
      return std::nullopt;
    }
    if (amount == 0) {
      return 0;
    }
    if (memo[amount] != -1) { // means already cached
      if (memo[amount] == INT_MAX) { // means inpossible
        return std::nullopt;
      }
      return memo[amount];
    }

    int minCount = INT_MAX;
    for (const auto &coin : coins) {
      auto res = findSmallestChange(coins, amount - coin, memo);
      if (res.has_value()) {
        minCount = std::min(minCount, 1 + res.value());
      }
    }

    memo[amount] = minCount;

    if (minCount == INT_MAX) {
      return std::nullopt; // not possible
    }
    return minCount;
  }
};