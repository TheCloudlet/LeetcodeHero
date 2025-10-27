// Leetcode 297. Serialize and Deserialize Binary Tree
// @tag: tree, dfs, bfs, neetcode150
// @difficulty: hard

/**
 * Definition for a binary tree node.
 * struct TreeNode {
 *     int val;
 *     TreeNode *left;
 *     TreeNode *right;
 *     TreeNode(int x) : val(x), left(NULL), right(NULL) {}
 * };
 */

#include <cassert>
#include <optional>
#include <queue>
#include <sstream>
#include <string>
#include <vector>

class Codec {
public:
  // Encodes a tree to a single string.
  std::string serialize(TreeNode *root) {
    if (!root) {
      return "null";
    }
    std::string result;
    std::queue<TreeNode *> nextNode;
    nextNode.push(root);

    while (!nextNode.empty()) {
      TreeNode *curr = nextNode.front();
      nextNode.pop();

      if (!curr) {
        result.append("null,");
        continue;
      }

      result.append(std::to_string(curr->val) + ",");
      nextNode.push(curr->left);
      nextNode.push(curr->right);
    }

    return result;
  }

  // Decodes your encoded data to tree.
  TreeNode *deserialize(std::string data) {
    std::vector<std::optional<int>> tokens;
    // 1. Find tokens and add to token list
    tokenize(data, tokens);
    assert(!tokens.empty());

    // 2. Construcct tree
    if (tokens[0] == std::nullopt) {
      return nullptr;
    }

    TreeNode *root = new TreeNode(tokens[0].value());
    std::queue<TreeNode *> nextNode;
    nextNode.push(root);

    std::size_t idx = 1; // iterate tokens
    while (!nextNode.empty()) {
      TreeNode *curr = nextNode.front();
      nextNode.pop();

      // Process left child
      if (idx < tokens.size() && tokens[idx].has_value()) {
        curr->left = new TreeNode(tokens[idx].value());
        nextNode.push(curr->left);
      }
      ++idx;

      // Process right child
      if (idx < tokens.size() && tokens[idx].has_value()) {
        curr->right = new TreeNode(tokens[idx].value());
        nextNode.push(curr->right);
      }
      ++idx;
    }
    assert(idx == tokens.size());

    return root;
  }

private:
  void tokenize(const std::string &data,
                std::vector<std::optional<int>> &tokens) {
    std::stringstream ss(data);
    std::string token;

    while (std::getline(ss, token, ',')) {
      if (token == "null") {
        tokens.push_back(std::nullopt);
      } else {
        tokens.push_back(std::stoi(token));
      }
    }
  }
};

// Your Codec object will be instantiated and called as such:
// Codec ser, deser;
// TreeNode* ans = deser.deserialize(ser.serialize(root));