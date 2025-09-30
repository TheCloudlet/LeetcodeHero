// Leetcode 206. Reverse Linked List
// @tag: linked-list
// @difficulty: easy

/**
 * Definition for singly-linked list.
 * struct ListNode {
 *     int val;
 *     ListNode *next;
 *     ListNode() : val(0), next(nullptr) {}
 *     ListNode(int x) : val(x), next(nullptr) {}
 *     ListNode(int x, ListNode *next) : val(x), next(next) {}
 * };
 */

#if defined(ITERATIVE)
class Solution {
public:
  ListNode *reverseList(ListNode *head) {
    if (!head) {
      nullptr;
    }
    ListNode *prev = nullptr;
    ListNode *curr = head;
    ListNode *peek;

    while (curr) {
      peek = curr->next;
      curr->next = prev;
      prev = curr;
      curr = peek;
    }
    return prev;
  }
};
#endif

#if defined(RECURSIVE)
class Solution {
public:
  ListNode *reverseList(ListNode *head) {
    // Base case: empty list or single node
    if (!head || !head->next) {
      return head;
    }

    // Recursively reverse the rest of the list
    ListNode *newHead = reverseList(head->next);

    // Reverse the current connection
    head->next->next = head;
    head->next = nullptr;

    return newHead;
  }
}
#endif