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

// NOTE: Alternative O(1) Space Solution
//
// Just as a point of interest, there's a clever but less intuitive solution
// that uses O(1) extra space by modifying the list in-place. It involves
// three passes:
//
// 1. Weave: Create a copy of each node and place it immediately after its
// original node. A -> B becomes A -> A' -> B -> B'.
//
// 2. Set Random Pointers: For each original node `curr`, its copy's random
// pointer can be found via `curr->next->random = curr->random->next`.
//
// 3. Unweave: Separate the combined list back into two distinct lists.