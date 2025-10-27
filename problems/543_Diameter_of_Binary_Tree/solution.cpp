// Leetcode 543. Diameter of Binary Tree
// @tag: tree, bfs, binary-tree, neetcode150
// @difficulty: easy

// NOTE:
// I did not flash the solution during the first view of the problem.
// But I think through the process and found out it is just a recursion or FOLD.

#include <algorithm>
#include <utility>

class Solution {
public:
  int diameterOfBinaryTree(TreeNode *root) {
    auto [depth, diameter] = getDepthAndDiameter(root);
    return diameter;
  }

private:
  // return (depth, diameter)
  std::pair<int, int> getDepthAndDiameter(TreeNode *root) {
    if (!root) {
      return {0, 0};
    }

    auto [depthL, diameterL] = getDepthAndDiameter(root->left);
    auto [depthR, diameterR] = getDepthAndDiameter(root->right);

    int currDepth = std::max(depthL, depthR) + 1;
    int currDiameter = std::max({depthL + depthR, diameterL, diameterR});

    return {currDepth, currDiameter};
  }
};