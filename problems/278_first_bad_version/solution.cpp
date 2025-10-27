// Leetcode 278. First Bad Version
// Binary Search for Minimal Satisfying Condition
//
// Goal:
//   Given a version control system with versions 1 to n,
//   and a function isBadVersion(version) that returns whether
//   a version is bad, find the first bad version.
//
// Strategy:
//   Use binary search to minimize the number of calls to isBadVersion.
//   The first bad version is the lowest index such that isBadVersion(i) == true.
//
//   Use [left, right) style binary search:
//     - If version is not bad, search in right half (left = mid + 1)
//     - If version is bad, it might be the answer → search in left half (right = mid)
//
// Return:
//   The index of the first bad version (left == right at termination).
//
// Time Complexity:
//   O(log n) binary search calls to isBadVersion()


// The API isBadVersion is defined for you.
// bool isBadVersion(int version);

class Solution {
public:
  int firstBadVersion(int n) {
    int left = 0, right = n;
    int mid;
    while (left < right) {
      mid = left + (right - left) / 2;
      if (isBadVersion(mid) == false) {
        left = mid + 1;
      } else {
        right = mid;
      }
    }
    return left;
  }
};
