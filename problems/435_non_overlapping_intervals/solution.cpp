// Leetcode 435 Non-overlapping Intervals

// Note:
// My mind is completely blank doing this problem. The key here is that when we
// see overlapping intervals, we greedily pick the one that ends later to
// remove.
//
// Time Complexity: O(N log N)
// Space Complexity: O(1)
//   where N is the number of intervals

#include <algorithm>
#include <vector>

class Solution {
 public:
  int eraseOverlapIntervals(std::vector<std::vector<int>>& intervals) {
    if (intervals.empty()) {
      return 0;
    }

    std::sort(intervals.begin(), intervals.end(),
              [](const std::vector<int>& a, const std::vector<int>& b) {
                return a[0] < b[0];
              });

    int remove_count = 0;
    int prev_end = intervals[0][1];

    // Greedy remove the longer end when merging
    for (size_t i = 1; i < intervals.size(); ++i) {
      const int curr_start = intervals[i][0];
      const int curr_end = intervals[i][1];

      if (prev_end > curr_start) {
        prev_end = std::min(curr_end, prev_end);
        ++remove_count;
      } else {
        prev_end = curr_end;
      }
    }

    return remove_count;
  }
};