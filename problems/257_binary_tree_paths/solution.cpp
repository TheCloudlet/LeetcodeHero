// Leetcode 257. Binary Tree Paths

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

#include <cassert>
#include <string>
#include <vector>

class Solution {
 public:
  std::vector<std::string> binaryTreePaths(TreeNode* root) {
    if (!root) return {};

    std::vector<std::string> ans;
    std::string path_stack;

    auto dfs = [&](auto& self, const TreeNode* curr) -> void {
      assert(curr);

      const std::string tmp = "->" + to_string(curr->val);
      path_stack += tmp;

      if (!curr->left && !curr->right) {
        ans.push_back(path_stack.substr(2));
      }

      if (curr->left) self(self, curr->left);
      if (curr->right) self(self, curr->right);

      path_stack.resize(path_stack.size() - tmp.size());
    };

    dfs(dfs, root);

    return ans;
  }
};
