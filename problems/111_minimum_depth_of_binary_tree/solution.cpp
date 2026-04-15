// Leetcode 111. Minimum Depth of Binary Tree

// The most efficient way is to use BFS and stop early

/**
 * Definition for a binary tree node.
 * struct TreeNode {
 *     int val;
 *     TreeNode *left;
 *     TreeNode *right;
 *     TreeNode() : val(0), left(nullptr), right(nullptr) {}
 *     TreeNode(int x) : val(x), left(nullptr), right(nullptr) {}
 *     TreeNode(int x, TreeNode *left, TreeNode *right) : val(x), left(left),
 * right(right) {}
 * };
 */

#if defined(DFS)
#include <algorithm>
#include <climits>

class Solution {
 public:
  int minDepth(TreeNode* root) {
    if (!root) return 0;

    if (!root->left && !root->right) return 1;

    const int left_min = root->left ? minDepth(root->left) : INT_MAX;
    const int right_min = root->right ? minDepth(root->right) : INT_MAX;

    return 1 + std::min(left_min, right_min);
  }
};
#endif

#if defined(DFS)
#include <algorithm>
#include <cassert>
#include <vector>

class Solution {
 public:
  int minDepth(TreeNode* root) {
    if (!root) return 0;

    std::vector<TreeNode*> next_level;
    std::vector<TreeNode*> curr_level;

    int level_count = 0;
    curr_level.push_back(root);

    while (!curr_level.empty()) {
      ++level_count;
      const int n = static_cast<int>(curr_level.size());

      for (int i = 0; i < n; ++i) {
        const TreeNode* node = curr_level[i];
        if (!node->left && !node->right) return level_count;

        if (node->left) next_level.push_back(node->left);
        if (node->right) next_level.push_back(node->right);
      }
      std::swap(curr_level, next_level);
      next_level.clear();
    }

    assert(false && "Should not reach here");
    return -1;
  }
};
#endif
