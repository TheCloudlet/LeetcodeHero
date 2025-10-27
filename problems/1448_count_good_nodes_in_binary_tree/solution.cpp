// Leetcode 1448. Count Good Nodes in Binary Tree
// @tag: tree, neetcode150
// @difficulty: medium

// Note:
// This contest problem is fun!

#include <algorithm> // std::max
#include <climits>   // INT_MIN

class Solution {
public:
  int goodNodes(TreeNode *root) { return countGoodNode(root, INT_MIN); }

  int countGoodNode(TreeNode *node, int prevMax) {
    if (!node) {
      return 0;
    }

    int goodCount = (node->val >= prevMax);

    int currMax = std::max(prevMax, node->val);

    goodCount += countGoodNode(node->left, currMax);
    goodCount += countGoodNode(node->right, currMax);

    return goodCount;
  }
};