// NOTE:
// 1. Loop decttion
// 2. Lamda function

#include <functional>
#include <vector>

// Create an ajacent list and check whether we have loop
class Solution {
public:
    bool canFinish(int numCourses,
                   std::vector<std::vector<int>>& prerequisites) {
        std::vector<std::vector<int>> adj(numCourses);

        // Transfer prerequisites to adjacent list
        for (const auto &pair : prerequisites) {
            auto a = pair.at(0);
            auto b = pair.at(1);
            adj[b].emplace_back(a);
        }

        std::vector<int> state(numCourses); // 0: not visited, 1: visiting
                                            // 2: visisted
        std::function<bool(int)> dfs = [&](int u) -> bool {
            if (state[u] == 1) {
                return true; // cound a cycle
            }
            if (state[u] == 2) {
                return false;
            }
            state[u] = 1; // mark as visiting
            for (const int &v : adj[u]) {
                if (dfs(v)) {
                    return true;
                }
            }
            state[u] = 2; // mark as visited
            return false;
        };

        for (int i = 0; i < numCourses; ++i) {
            if (dfs(i)) {
                return false;
            }
        }
        return true;
    }
};
