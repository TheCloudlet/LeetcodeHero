// Leetcode 3728. Stable Subarrays With Equal Boundary and Interior Sum
// @tag: prefix-sum, subarray, weekly-contest
// @difficulty: medium

// For sub-array problem there are 3 ways to solve:
// 1. Sliding window
// 2. DP / prefix-sum
// 3. Brute force

// For the first time, I didn't see prefix-sum could be used here.
// So I directly used brute-force to solve it. Of course, TLE.
// Time complexity O(N^3)
//
// Second try, I used prefix-sum to optimize the inner sum calculation.
// Time complexity O(N^2)
//
// Third try, I used hashmap to group the same values together.
// So that we can skip the first condition check (capacity[i] == capacity[j]).
// Time complexity O(N^2) in worst case but faster in practice.
//
// The final solution is as below. Provided by Gemini AI.
//
// Key concept:
// Transform an iteration problem into a find problem.

#include <map>
#include <unordered_map>
#include <vector>

class Solution {
 public:
  long long countStableSubarrays(std::vector<int>& capacity) {
    int n = capacity.size();
    if (n < 3) {
      return 0;
    }

    long long valid_count = 0;

    std::vector<long long> prefix_sum(n);
    prefix_sum[0] = capacity[0];
    for (int i = 1; i < n; ++i) {
      prefix_sum[i] = prefix_sum[i - 1] + capacity[i];
    }

    std::map<std::pair<int, long long>, int> freq_map;

    for (int r = 2; r < n; ++r) {
      // Key point:
      // Add the potential left boundary at position (r-2) to our map for later
      // lookup. For example, when prefix_sum[r-1] is 6 and capacity[r] is 2,
      // we calculate target_pl = 6 - 2 = 4, then search for how many previous
      // positions have {capacity[l] == 2, prefix_sum[l] == 4}.
      // The number of such pairs indicates how many valid subarrays end at r.
      int l = r - 2;
      int l_val = capacity[l];
      long long l_p = prefix_sum[l];
      freq_map[{l_val, l_p}]++;

      int r_val = capacity[r];
      long long target_pl = prefix_sum[r - 1] - r_val;

      if (freq_map.count({r_val, target_pl})) {
        valid_count += freq_map.at({r_val, target_pl});
      }
    }

    return valid_count;
  }
}