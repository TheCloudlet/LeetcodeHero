// Leetcode 230. Kth Smallest Element in a BST
// @tag: tree, inorder-traversal, neetcode150
// @difficulty: medium

#if define(RECURSIVE)
class Solution {
 public:
  int kthSmallest(TreeNode* root, int k) {
    if (!root || k <= 0) return -1;

    int count = 0;
    int kth_num = -1;

    auto inorder = [&count, &kth_num, k](auto self, TreeNode* root) {
      if (!root) return;
      if (count >= k) return;

      self(self, root->left);

      if (++count == k) {
        kth_num = root->val;
        return;
      }

      self(self, root->right);
    };

    inorder(inorder, root);

    return kth_num;
  }
};
#endif

#if define(ITERATIVE)
class Solution {
 public:
  int kthSmallest(TreeNode* root, int k) {
    std::stack<TreeNode*> nodeStack;
    TreeNode* curr = root;

    while (curr != nullptr || !nodeStack.empty()) {
      // 1. Go as far left as possible from the current node.
      while (curr != nullptr) {
        nodeStack.push(curr);
        curr = curr->left;
      }

      // 2. At this point, curr is null. Pop the most recently
      // added node; this is the next in the inorder sequence.
      curr = nodeStack.top();
      nodeStack.pop();

      // 3. Process the node.
      if (--k == 0) {
        return curr->val;
      }

      // 4. Finally, try to go right. The outer loop will then
      // find the leftmost node of this new subtree.
      curr = curr->right;
    }
    return -1;  // Should not be reached
  }
};
#endif
