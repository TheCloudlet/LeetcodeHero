// Leetcode 96. Unique Binary Search Trees

#include <cassert>
#include <vector>

class Solution {
public:
    Solution() {
        cache.resize(20, 0);
        cache[0] = 1; // base case
        cache[1] = 1; // 1 node, 1 BST
    }
    int numTrees(int n) {
        assert(n >= 0 && n <= 19);
        if (cache[n] != 0) {
            return cache[n];
        }

        int result = 0;
        // split into two halves
        for (int i = 0; i < n; ++i) {
            result += numTrees(i) * numTrees(n - i - 1); // mistake: used `+`
        }

        cache[n] = result;
        return result;
    }

private:
    std::vector<int> cache;
};
