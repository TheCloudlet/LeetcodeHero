// LeetCode 399: Evaluate Division
//
// Time Complexity:
//   Graph creation: O(E)
//   Query: O(Q * (V + E))
//
// Space Complexity: O(V + E)
//
// where
//   E = equations.size()
//   Q = queries.size()
//   V = number of graph vertices

#include <cassert>
#include <queue>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>

class Solution {
 public:
  std::vector<double> calcEquation(
      const std::vector<std::vector<std::string>>& equations,
      const std::vector<double>& values,
      const std::vector<std::vector<std::string>>& queries) {
    size_t input_size = equations.size();
    assert(input_size == values.size());

    std::unordered_map<std::string, std::vector<std::pair<std::string, double>>>
        graph;

    // Construct division graph
    for (size_t i = 0; i < input_size; ++i) {
      const std::string& from = equations[i][0];
      const std::string& to = equations[i][1];
      const double val = values[i];
      graph[from].emplace_back(to, val);
      graph[to].emplace_back(from, 1.0 / val);
    }

    // Query result
    std::vector<double> out;
    for (const auto& query : queries) {
      const std::string& from = query[0];
      const std::string& to = query[1];
      out.push_back(bfsGraph(graph, from, to));
    }

    return out;
  }

 private:
  double bfsGraph(
      const std::unordered_map<
          std::string, std::vector<std::pair<std::string, double>>>& graph,
      const std::string& from, const std::string& to) {
    if (from == to) {
      return graph.count(from) ? 1.0 : -1.0;
    }

    if (!graph.count(from) || !graph.count(to)) {
      return -1.0;
    }

    std::unordered_set<std::string> visited;
    std::queue<std::pair<std::string, double>> q;

    q.emplace(from, 1.0);
    visited.emplace(from);

    while (!q.empty()) {
      const auto [node, val] = q.front();
      q.pop();

      if (node == to) {
        return val;
      }

      // Add its adj to queue
      const auto it = graph.find(node);
      if (it == graph.end()) {
        continue;
      }
      for (const auto& [adj_node, adj_val] : it->second) {
        if (!visited.count(adj_node)) {
          q.emplace(adj_node, adj_val * val);
          visited.emplace(adj_node);
        }
      }
    }

    return -1.0;
  }
};
