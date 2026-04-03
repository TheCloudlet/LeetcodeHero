// Leetcode 653. Two Sum IV - Input is a BST

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

#if defined(EAGER_EVALUATION)
#include <vector>

class Solution {
 public:
  bool findTarget(TreeNode* root, int k) {
    if (!root) return false;

    std::vector<int> sorted_value;
    inorder(root, sorted_value);

    int left = 0;
    int right = static_cast<int>(sorted_value.size()) - 1;

    while (left < right) {
      int sum = sorted_value[right] + sorted_value[left];

      if (sum == k)
        return true;
      else if (sum < k)
        ++left;
      else
        --right;
    }

    return false;
  }

 private:
  void inorder(TreeNode* root, std::vector<int>& val) {
    if (!root) return;

    inorder(root->left, val);
    val.push_back(root->val);
    inorder(root->right, val);
  }
};
#endif

#if defined(LAZY_EVALUATION)
#include <stack>

// Abstraction Barrier: The BST Iterator
class BSTIterator {
 private:
  std::stack<TreeNode*> st;
  // true for ascending (Left -> Right), false for descending (Right -> Left)
  bool forward;

  // Helper function: push nodes all the way down to the extremum
  void push_all(TreeNode* node) {
    while (node) {
      st.push(node);
      node = forward ? node->left : node->right;
    }
  }

 public:
  BSTIterator(TreeNode* root, bool forward) : forward(forward) {
    push_all(root);
  }

  int next() {
    TreeNode* current = st.top();
    st.pop();

    // State transition: step once in the opposite direction,
    // then push all the way down again to find the next sequential element
    push_all(forward ? current->right : current->left);

    return current->val;
  }

  bool has_next() const { return !st.empty(); }
};

class Solution {
 public:
  bool findTarget(TreeNode* root, int k) {
    if (!root) return false;

    // Initialize two iterators acting as pointers:
    // One starting at the absolute minimum, the other at the absolute maximum
    BSTIterator left_iter(root, true);
    BSTIterator right_iter(root, false);

    int left_val = left_iter.next();
    int right_val = right_iter.next();

    // Standard Two-Pointer converging logic
    while (left_val < right_val) {
      int sum = left_val + right_val;
      if (sum == k) return true;

      if (sum < k) {
        // Greedy: advance to seek a larger value
        left_val = left_iter.next();
      } else {
        // Let go: retreat to seek a smaller value
        right_val = right_iter.next();
      }
    }

    return false;
  }
};
#endif
