// Leetcode 743 Network Delay Time
//
// Classic Dijkstra's

#include <algorithm>
#include <climits>
#include <queue>
#include <vector>

class Solution {
 public:
  int networkDelayTime(const std::vector<std::vector<int>>& times, int n,
                       int k) {
    // Construct adj_list {weight, to}
    std::vector<std::vector<std::pair<int, int>>> adj_list(n + 1);  // 1-indexed
    for (const auto& time : times) {
      const int from = time[0];
      const int to = time[1];
      const int weight = time[2];
      adj_list[from].push_back({weight, to});
    }

    std::vector<int> min_dist(n + 1, INT_MAX);
    std::vector<bool> visited(n + 1, false);

    std::priority_queue<std::pair<int, int>, std::vector<std::pair<int, int>>,
                        std::greater<>>
        min_heap;

    min_dist[k] = 0;
    min_heap.emplace(0, k);

    while (!min_heap.empty()) {
      auto [curr_weight, curr] = min_heap.top();
      min_heap.pop();

      // Skip stale entries (same node pushed multiple times)
      if (visited[curr]) continue;

      visited[curr] = true;

      for (const auto& [edge_weight, next_node] : adj_list[curr]) {
        int new_weight = curr_weight + edge_weight;

        if (new_weight < min_dist[next_node]) {     // Verify
          min_dist[next_node] = new_weight;         // Mutate
          min_heap.emplace(new_weight, next_node);  // Push
        }
      }
    }

    // Remember to `+1` since index 0 is dummy
    const int max_time =
        *std::max_element(min_dist.begin() + 1, min_dist.end());

    return max_time != INT_MAX ? max_time : -1;
  }
};
