// Leetcode 145 Binary Tree Preorder Traversal
// @tag: tree, dfs, traversal
// @difficulty: easy

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

// NOTE:
// Therefore, we can just do a reversed pre-order traversal

#include <stack>
#include <vector>

#if defined(solution_1)
class Solution {
 public:
  // Iteratively computes the post-order traversal of a binary tree.
  //
  // The approach is based on the observation that a post-order traversal
  // (Left -> Right -> Node) is the reverse of a pre-order-like traversal
  // with the order (Node -> Right -> Left).
  //
  // The algorithm first performs this modified pre-order traversal using a
  // single stack. To achieve the Node -> Right -> Left order, after processing
  // a node, its children are pushed onto the stack left-child first, then
  // right-child. Due to the stack's LIFO property, the right child is
  // guaranteed to be visited before the left child.
  //
  // The resulting sequence of node values is collected and then reversed to
  // produce the correct post-order traversal. This method avoids the complexity
  // of single-pass iterative solutions that require additional state (e.g., a
  // 'visited' flag) to track when a node's children have been explored.
  std::vector<int> postorderTraversal(TreeNode* root) {
    if (!root) {
      return {};
    }

    std::vector<int> result;
    std::stack<TreeNode*> traversalStack;
    traversalStack.push(root);

    while (!traversalStack.empty()) {
      TreeNode* curr = traversalStack.top();
      traversalStack.pop();
      result.push_back(curr->val);

      if (curr->left) {
        traversalStack.push(curr->left);
      }
      if (curr->right) {
        traversalStack.push(curr->right);
      }
    }

    std::reverse(result.begin(), result.end());

    return result;
  }
};
#endif

#if defined(solution_2)
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
class Solution {
 public:
  vector<int> postorderTraversal(TreeNode* root) {
    std::vector<int> out;

    auto helper = [&](auto self, TreeNode* root) {
      if (!root) {
        return;
      }

      self(self, root->left);
      self(self, root->right);

      out.push_back(root->val);
    };

    helper(helper, root);
    return out;
  }
};

#endif
