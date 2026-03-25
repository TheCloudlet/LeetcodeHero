// Leetcode 876 Middle of the Linked List

class Solution {
 public:
  ListNode* middleNode(ListNode* head) {
    ListNode* slow = head;
    ListNode* fast = head;

    while (fast && fast->next) {  // Made mistake here!
      slow = slow->next;
      fast = fast->next->next;
    }

    return slow;
  }
};
