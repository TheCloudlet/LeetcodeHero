// Leetcode 155. Min Stack
// @tag: design, stack, monotonic stack
// @difficulty: medium

// NOTE:
// The key is to maintain a monotonic stack to track the minimum value in the
// specific stack interval.
//
//
// Push sequence: [3, 1, 4, 1, 2]
//
// mainStack:  [3, 1, 4, 1, 2]  ← All elements
// monoMin:    [3, 1,    1   ]  ← Only interval minimums
//              ↑  ↑     ↑
//            min  min   min for [3,4,1,4,2]
//            for  for
//            [3] [3,1]

#include <cassert>
#include <stack>

class MinStack {
 private:
  std::stack<int> main_st;
  std::stack<int> min_st;

 public:
  MinStack() = default;

  void push(int val) {
    main_st.push(val);

    if (min_st.empty() || min_st.top() >= val) {
      min_st.push(val);
    }
  }

  void pop() {
    assert(!main_st.empty());
    int removed_val = main_st.top();
    main_st.pop();

    if (min_st.top() == removed_val) {
      min_st.pop();
    }
  }

  int top() {
    assert(!main_st.empty());
    return main_st.top();
  }

  int getMin() {
    assert(!min_st.empty());
    return min_st.top();
  }
};

/**
 * Your MinStack object will be instantiated and called as such:
 * MinStack* obj = new MinStack();
 * obj->push(val);
 * obj->pop();
 * int param_3 = obj->top();
 * int param_4 = obj->getMin();
 */
