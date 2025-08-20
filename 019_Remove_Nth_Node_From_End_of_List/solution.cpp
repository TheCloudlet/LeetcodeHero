// Leetcode 19. Remove Nth Node From End of List
// @tag:
// @difficulty: medium

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

ListNode *removeNthFromEnd(ListNode *head, int n) {
  ListNode **fast = &head;
  ListNode **slow = &head;

  int steps_taken = 0;
  for (; steps_taken < n && *fast; ++steps_taken) {
    fast = &(*fast)->next;
  }

  if (steps_taken < n) {
    return head;
  }

  while (*fast) {
    fast = &(*fast)->next;
    slow = &(*slow)->next;
  }

  ListNode *nodeToDelete = *slow;
  *slow = (*slow)->next;
  delete nodeToDelete;

  return head;
}