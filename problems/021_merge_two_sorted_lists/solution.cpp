// Leetcode 21. Merge Two Sorted Lists
// @tag: linked-list, neetcode-150
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

#if defined(SIMPLE)
class Solution {
public:
  ListNode *mergeTwoLists(ListNode *list1, ListNode *list2) {
    if (!list1) {
      return list2;
    }
    if (!list2) {
      return list1;
    }
    ListNode *dummy = new ListNode();
    ListNode *last = dummy;

    while (list1 && list2) {
      if (list1->val < list2->val) {
        last->next = list1;
        last = last->next;
        list1 = list1->next;
      } else {
        last->next = list2;
        last = last->next;
        list2 = list2->next;
      }
    }

    last->next = *list1 ? list1 : list2;

    ListNode *result = dummy->next;
    delete dummy;

    return result;
  }
};
#endif

#if defined(INDIRECT)
class Solution {
public:
  ListNode *mergeTwoLists(ListNode *list1, ListNode *list2) {
    ListNode *result = nullptr;
    ListNode **indirect = &result;

    while (list1 && list2) {
      if (list1->val < list2->val) {
        *indirect = list1;
        list1 = list1->next;
      } else {
        *indirect = list2;
        list2 = list2->next;
      }
      indirect = &((*indirect)->next);
    }

    // Attach remaining nodes
    *indirect = list1 ? list1 : list2;

    return result;
  }
};
#endif