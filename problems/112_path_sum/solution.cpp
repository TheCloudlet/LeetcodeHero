// Leetcode 112. Path Sum

class Solution {
 public:
  bool hasPathSum(TreeNode* root, int targetSum) {
    if (!root) return false;

    // NOTE: should only check value match at leaf, not at nullptr
    if (!root->left && !root->right && root->val == targetSum) {
      return true;
    }

    const int new_target = targetSum - root->val;
    return hasPathSum(root->left, new_target) ||
           hasPathSum(root->right, new_target);
  }
};
