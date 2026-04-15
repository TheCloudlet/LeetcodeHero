// Leetcode 637. Average of Levels in Binary Tree

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


#if defined(BFS_QUEUE)
#include <queue>
#include <vector>

class Solution {
 public:
  std::vector<double> averageOfLevels(TreeNode* root) {
    if (!root) return {};

    std::vector<double> result;

    std::frontier<TreeNode*> frontier;
    frontier.push(root);

    while (!frontier.empty()) {
      const int level_size = static_cast<int>(frontier.size());
      double level_sum = 0;

      for (int i = 0; i < level_size; ++i) {
        const TreeNode* node = frontier.front();
        frontier.pop();

        level_sum += node->val;

        if (node->left) frontier.push(node->left);
        if (node->right) frontier.push(node->right);
      }

      result.push_back(level_sum / static_cast<double>(level_size));
    }

    return result;
  }
};
#endif

#if defined(BFS_DOUBLE_BUFFERING)
#include <vector>

class Solution {
 public:
  std::vector<double> averageOfLevels(TreeNode* root) {
    if (!root) return {};

    std::vector<double> result;

    std::vector<TreeNode*> curr_level;
    std::vector<TreeNode*> next_level;

    curr_level.push_back(root);

    while (!curr_level.empty()) {
      const int level_size = static_cast<int>(curr_level.size());
      double level_sum = 0;

      for (int i = 0; i < level_size; ++i) {
        const TreeNode* node = curr_level[i];

        level_sum += node->val;

        if (node->left) next_level.push_back(node->left);
        if (node->right) next_level.push_back(node->right);
      }

      result.push_back(level_sum / static_cast<double>(level_size));

      std::swap(curr_level, next_level);
      next_level.clear();
    }

    return result;
  }
};
#endif
