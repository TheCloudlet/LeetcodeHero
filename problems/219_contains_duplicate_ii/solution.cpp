// Leetcode 219. Contains Duplicate II

#if defined(MY)
// Problem:
// 1. Implicit Memory Bloat (The "Dead Weight" Map)
//    Decrementing `freq[nums[left]]` to 0 when elements exit the window does
//    not remove the key from the `std::unordered_map`. Over time, the map
//    accumulates stale `{key: 0}` entries. This degrades space complexity from
//    an ideal O(min(N, K)) to a worst-case O(N), triggering unnecessary
//    allocations and expensive rehashes. If a map is used, we must explicitly
//    call `freq.erase()` when the frequency hits 0. (Alternatively,
//    `std::unordered_set` is cleaner).
//
// 2. Redundant Hash Lookups (Cache-Unfriendly Pointer Chasing)
//    Performing `++freq[nums[right]]` followed by `freq[nums[right]] > 1`
//    forces two hash lookups on the same element per iteration. Since
//    `std::unordered_map` uses chaining (bucket array + linked list), each
//    lookup is a cache-unfriendly memory trip. Reducing this to a single lookup
//    is a major performance win on the hot path.
#include <unordered_map>
#include <vector>

class Solution {
 public:
  bool containsNearbyDuplicate(const std::vector<int>& nums, int k) {
    int len = static_cast<int>(nums.size());

    std::unordered_map<int, int> freq;

    int left = 0;
    for (int right = 0; right < len; ++right) {
      ++freq[nums[right]];

      while (right - left > k) {
        --freq[nums[left]];
        ++left;
      }

      // Valid window
      if (freq[nums[right]] > 1) return true;
    }

    return false;
  }
};
#endif

#if defined(BETTER)
class Solution {
 public:
  bool containsNearbyDuplicate(const std::vector<int>& nums, int k) {
    std::unordered_map<int, int> last_seen;
    int len = static_cast<int>(nums.size());

    for (int i = 0; i < len; ++i) {
      int num = nums[i];
      if (last_seen.count(num) && i - last_seen[num] <= k) {
        return true;
      }
      last_seen[num] = i;
    }

    return false;
  }
};
#endif
