// Leetcode 347 Top K Frequent Elements

#if defined(trivial)
// Time Complexity: O(N log N)
// Space Complexity: O(N)
//   where N is the number of elements in the input array.

#include <queue>
#include <unordered_map>
#include <vector>

class Solution {
 public:
  std::vector<int> topKFrequent(std::vector<int>& nums, int k) {
    std::unordered_map<int, int> freq;

    for (const auto& num : nums) {
      ++freq[num];
    }

    auto cmp = [](const std::pair<int, int>& lhs,
                  const std::pair<int, int>& rhs) {
      return lhs.second < rhs.second;
    };

    // Be super careful for declaration of priority queue. Error prone.
    std::priority_queue<std::pair<int, int>,
        std::vector<std::pair<int, int>>, decltype(cmp)> maxHeap(cmp);

    for (const auto& elem : freq) {
      maxHeap.emplace(elem);
    }

    std::vector<int> result;
    result.reserve(k); // Reserve capacity upfront to avoid reallocations

    for (int i = 1; i <= k; ++i) {
      // For primitive types, prefer push_back: clearer intent (copying value)
      // and avoids potential narrowing conversion issues with emplace_back
      result.push_back(maxHeap.top().first);
      maxHeap.pop();
    }

    return result;
  }
};
#endif

#if defined(optimized_space)
// Time Complexity: O(N log K)
// Space Complexity: O(K)
//   where N is the number of elements in the input array
//         K is the number of frequent elements.

#include <queue>
#include <unordered_map>
#include <vector>

class Solution {
 public:
  std::vector<int> topKFrequent(std::vector<int>& nums, int k) {
    std::unordered_map<int, int> freq;

    for (const auto& num : nums) {
      ++freq[num];
    }

    auto cmp = [](const std::pair<int, int>& lhs,
                  const std::pair<int, int>& rhs) {
      return lhs.second > rhs.second; // change to minHeap
    };

    std::priority_queue<std::pair<int, int>,
        std::vector<std::pair<int, int>>, decltype(cmp)> minHeap(cmp);

    for (const auto& elem : freq) {
      minHeap.emplace(elem);
      if (minHeap.size() > static_cast<size_t>(k)) {
        minHeap.pop();
      }
    }

    std::vector<int> result;
    result.reserve(k);

    while (!minHeap.empty()) {
      result.push_back(minHeap.top().first);
      minHeap.pop();
    }

    return result;
  }
};
#endif
