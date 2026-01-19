// Leetcode 057 Insert Interval

// Note:
// Unlike LC56, this is not a single fold over all intervals. We split the work
// into three phases: copy before insertion, merge overlaps, then append the
// rest.

#include <algorithm>
#include <vector>

class Solution {
 public:
  std::vector<std::vector<int>> insert(std::vector<std::vector<int>>& intervals,
                                       std::vector<int>& newInterval) {
    std::vector<std::vector<int>> result;
    result.reserve(intervals.size() + 1);

    size_t n = intervals.size();
    size_t i = 0;

    // Copy before insertion
    // Note: I wrote `intervals[i][0] < newInterval[0]` in the first attempt.
    while (i < n && intervals[i][1] < newInterval[0]) {
      result.emplace_back(intervals[i]);
      ++i;
    }

    // Merge overlaps
    while (i < n && newInterval[1] >= intervals[i][0]) {
      // Note: didn't write the next line in the first attempt.
      newInterval[0] = std::min(intervals[i][0], newInterval[0]);
      newInterval[1] = std::max(intervals[i][1], newInterval[1]);
      ++i;
    }

    result.emplace_back(newInterval);

    // Append the rest
    while (i < n) {
      result.emplace_back(intervals[i]);
      ++i;
    }

    return result;
  }
};
