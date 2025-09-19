// Leetcode 199. Binary Tree Right Side View
// @tag: tree, bfs, binary-tree, neetcode150
// @difficulty: medium

#include <queue>
#include <vector>

class Solution {
public:
  std::vector<int> rightSideView(TreeNode *root) {
    if (!root) {
      return {};
    }

    std::vector<int> result;
    std::queue<TreeNode *> levelNodeQueue;
    levelNodeQueue.push(root);

    while (!levelNodeQueue.empty()) {
      int levelCount = levelNodeQueue.size();
      for (int idx = 0; idx < levelCount; ++idx) {
        TreeNode *node = levelNodeQueue.front();
        levelNodeQueue.pop();

        if (node->left) {
          levelNodeQueue.push(node->left);
        }
        if (node->right) {
          levelNodeQueue.push(node->right);
        }

        if (idx == levelCount - 1) {
          result.push_back(node->val);
        }
      }
    }
    return result;
  }
};