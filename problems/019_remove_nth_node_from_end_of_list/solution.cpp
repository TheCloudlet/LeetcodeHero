// Leetcode 19. Remove Nth Node From End of List
// @tag: neetcode150, linked-list, two-pointers
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
class Solution {
 public:
  ListNode* removeNthFromEnd(ListNode* head, int n) {
    if (n == 0) return head;

    ListNode** slow = &head;
    ListNode* fast = head;

    int count = 0;
    while (fast && count < n) {
      fast = fast->next;
      ++count;
    }

    if (count < n) return head;

    while (fast) {
      slow = &((*slow)->next);
      fast = fast->next;
    }

    ListNode* node_to_delete = *slow;
    *slow = (*slow)->next;
    delete node_to_delete;

    return head;
  }
};
