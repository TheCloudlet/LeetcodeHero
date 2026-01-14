// Leetcode 124. Binary Tree Maximum Path Sum
// @tag: tree, depth-first search, recursion, neetcode150
// @difficulty: hard

#include <algorithm>
#include <limits>
#include <utility>

class Solution {
 public:
  int maxPathSum(TreeNode* root) {
    if (!root) {
      return 0;
    }
    auto [global_max, _] = calculateMaxPath(root);
    return global_max;
  }

 private:
  // Return: {Global Max, Path Contribution (>=0)}
  // @global_max: global max path
  // @path_contrib: max_path from root to leaf
  std::pair<int, int> calculateMaxPath(const TreeNode* root) {
    if (!root) {
      return {INT_MIN, 0};
    }

    const auto [l_max, l_contrib] = calculateMaxPath(root->left);
    const auto [r_max, r_contrib] = calculateMaxPath(root->right);

    //         A (val: 10)    a->left  => [-100, 0]
    //        /  \            a->right => [5, 5]
    //    -100    5

    int current_arch_sum = root->val + l_contrib + r_contrib;
    int new_global_max = std::max({l_max, r_max, current_arch_sum});
    int new_contrib = std::max(0, root->val + std::max(l_contrib, r_contrib));

    return {new_global_max, new_contrib};
  }
};

