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
  std::stack<int> mainStack;
  std::stack<int> monoMin; // Monotonic min stack.
                           // getMin() works when mainStack is not empty

public:
  MinStack() {}

  void push(int val) {
    if (monoMin.empty() || val <= monoMin.top()) {
      monoMin.push(val);
    }
    mainStack.push(val);
  }

  void pop() {
    int removedVal = mainStack.top();
    mainStack.pop();
    if (!monoMin.empty() && monoMin.top() == removedVal) {
      monoMin.pop();
    }
  }

  int top() {
    assert(!mainStack.empty());
    return mainStack.top();
  }

  int getMin() {
    assert(!monoMin.empty());
    return monoMin.top();
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