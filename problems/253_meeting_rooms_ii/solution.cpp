// Leetcode 253 Meeting Room II

// Time Complexity: O(N log N)
// Space Complexity: O(N)
//   where N is the size of intervals

#include <algorithm>
#include <queue>
#include <vector>

/**
 * Definition of Interval:
 * class Interval {
 * public:
 *     int start, end;
 *     Interval(int start, int end) {
 *         this->start = start;
 *         this->end = end;
 *     }
 * }
 */

class Solution {
 public:
  int minMeetingRooms(std::vector<Interval>& intervals) {
    if (intervals.empty()) {
      return 0;
    }

    std::sort(intervals.begin(), intervals.end(),
              [](const Interval& lhs, const Interval& rhs) {
                return lhs.start < rhs.start;
              });

    std::priority_queue<int, std::vector<int>, std::greater<int>> rooms;

    for (const auto& interval : intervals) {
      // If the earliest meeting ends before/at this start, reuse that room.
      // The empty check avoids accessing rooms.top() before any room is added.
      if (!rooms.empty() && rooms.top() <= interval.start) {
        rooms.pop();
      }

      rooms.push(interval.end);
    }

    return rooms.size();
  }
};
