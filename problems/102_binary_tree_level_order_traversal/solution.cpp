// Leetcode 102 Maximum Depth of Binary Tree
// @tag: tree, traversal, neetcode150
// @difficulty: medium

class Solution {
public:
  std::vector<std::vector<int>> levelOrder(TreeNode *root) {
    if (!root) {
      return {}; // Handles the null tree case cleanly
    }

    std::vector<std::vector<int>> result;
    std::queue<TreeNode *> nodeQueue;
    nodeQueue.push(root);

    while (!nodeQueue.empty()) {
      int levelSize = nodeQueue.size(); // NOTE: Know exactly level size
      std::vector<int> currentLevel;

      for (int i = 0; i < levelSize; ++i) {
        TreeNode *node = nodeQueue.front();
        nodeQueue.pop();

        currentLevel.push_back(node->val);

        if (node->left) {
          nodeQueue.push(node->left);
        }
        if (node->right) {
          nodeQueue.push(node->right);
        }
      }
      result.push_back(currentLevel);
    }

    return result;
  }
};