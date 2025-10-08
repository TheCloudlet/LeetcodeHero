// Leetcode 138. Copy List with Random Pointer
// @tag: linked-list, neetcode150
// @difficulty: medium

/*
// Definition for a Node.
class Node {
public:
    int val;
    Node* next;
    Node* random;

    Node(int _val) {
        val = _val;
        next = NULL;
        random = NULL;
    }
};
*/

class Solution {
public:
  Node *copyRandomList(Node *head) {
    if (!head) {
      return nullptr;
    }

    std::unordered_map<Node *, Node *> map;

    Node *newHead = nullptr;
    Node **indirect = &newHead;

    // First pass: create nodes and build mapping.
    Node *curr = head;
    while (curr) {
      Node *newNode = new Node(curr->val);
      map[curr] = newNode;
      *indirect = newNode;
      curr = curr->next;
      indirect = &((*indirect)->next);
    }

    // Second pass: set random pointers.
    curr = head;
    while (curr) {
      if (map.find(curr) != map.end() && map.find(curr->random) != map.end()) {
        Node *newNode = map[curr];
        newNode->random = map[curr->random];
      }
      curr = curr->next;
    }

    return newHead;
  }
};