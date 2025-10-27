// Leetcode 23. Merge k Sorted Lists
// @tag: linked-list
// @difficulty: hard

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

#include <queue>
#include <vector>

class Solution {
 public:
  ListNode* mergeKLists(std::vector<ListNode*>& lists) {
    ListNode* new_head = nullptr;
    ListNode** indirect = &new_head;

    auto comparator = [](ListNode* a, ListNode* b) { return a->val > b->val; };
    std::priority_queue<ListNode*, std::vector<ListNode*>, decltype(comparator)>
        pq(comparator);

    for (const auto& list : lists) {
      if (list) {  // This if is missing when first try
        pq.push(list);
      }
    }

    while (!pq.empty()) {
      ListNode* to_add = pq.top();
      pq.pop();

      *indirect = to_add;
      indirect = &(*indirect)->next;

      if (to_add->next) {
        pq.push(to_add->next);
      }
    }

    return new_head;
  }
};