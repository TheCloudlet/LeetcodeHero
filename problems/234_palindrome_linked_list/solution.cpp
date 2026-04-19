// Leetcode 234. Palindrome Linked List

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

// Two solutions
//
// 1) Two passes:
//    - Travese and check full lenght
//    - Use stack to check
//
// 2) Fast-slow pointer + reverse + traversal

#if defined(STACK)
#include <cassert>
#include <stack>

class Solution {
 public:
  bool isPalindrome(ListNode* head) {
    // CAUTION: Better think 0 node, 1 node ...
    if (!head || !head->next) return true;

    ListNode* p = head;
    int node_count = 0;
    while (p) {
      ++node_count;
      p = p->next;
    }

    std::stack<int> st;
    p = head;

    for (int i = 0; i < node_count / 2; ++i) {
      st.push(p->val);
      p = p->next;
    }

    // Mistake: wrote (% 2 == 0)
    if (node_count % 2 == 1) p = p->next;  // Skip the middle right point if odd

    while (p) {
      assert(!st.empty());

      if (p->val != st.top()) return false;

      st.pop();
      p = p->next;
    }

    return true;
  }
};
#endif

#if defined(FAST_SLOW_REV)

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
  bool isPalindrome(ListNode* head) {
    if (!head || !head->next) return true;

    ListNode* fast = head;
    ListNode* slow = head;

    while (fast->next && fast->next->next) {
      slow = slow->next;
      fast = fast->next->next;
    }

    ListNode* prev = nullptr;
    ListNode* curr = slow->next;

    while (curr) {
      ListNode* nxt = curr->next;
      curr->next = prev;
      prev = curr;
      curr = nxt;
    }

    ListNode const* p1 = head;
    ListNode const* p2 = prev;

    while (p1 && p2) {
      if (p1->val != p2->val) return false;

      p1 = p1->next;
      p2 = p2->next;
    }

    return true;
  }
};
#endif
