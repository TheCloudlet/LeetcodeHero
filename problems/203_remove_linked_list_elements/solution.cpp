// Leetcode 203. Remove Linked List Elements

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
  ListNode* removeElements(ListNode* head, int val) {
    ListNode** nextp = &head;

    while (*nextp) {
      if ((*nextp)->val == val) {
        ListNode* to_remove = *nextp;
        *nextp = to_remove->next;
        delete to_remove;
        continue;
      }

      nextp = &((*nextp)->next);
    }

    return head;
  }
};
