#include <cassert>
#include <queue>
#include <vector>

class Solution {
public:
    std::vector<int> findOrder(int numCourses,
                     std::vector<std::vector<int>>& prerequisites) {
        std::vector<std::vector<int>> adjList(numCourses);
        std::vector<int> inDegree(numCourses, 0);
        for (const auto &pair : prerequisites) {
            int to = pair.at(0); // to
            int from = pair.at(1); // from
            adjList[from].emplace_back(to);
            ++inDegree[to];
        }

        std::vector<int> result;
        std::queue<int> q;
        // Khan's algorithm
        for (int i = 0; i < numCourses; i++) {
            if (0 == inDegree[i]) {
                q.push(i);
            }
        }
        while (!q.empty()) {
            int u = q.front();
            q.pop();
            result.emplace_back(u);
            for (const auto& v : adjList[u]) {
                if (0 == --inDegree[v]) {
                    q.push(v);
                }
            }
        }
        if (result.size() < numCourses) {
            return {}; // contains cycle
        }
        return result;
    }
};
