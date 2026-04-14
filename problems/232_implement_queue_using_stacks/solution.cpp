// Leetcode 232. Implement Queue using Stacks

#include <cassert>
#include <stack>

class MyQueue {
 private:
  std::stack<int> in_stack;
  std::stack<int> out_stack;

 public:
  MyQueue() = default;

  void pour() {
    if (out_stack.empty()) {  // Wrong here
      while (!in_stack.empty()) {
        out_stack.push(in_stack.top());
        in_stack.pop();
      }
    }
  }

  void push(int x) { in_stack.push(x); }

  int pop() {
    pour();
    assert(!out_stack.empty());

    int ret = out_stack.top();
    out_stack.pop();
    return ret;
  }

  int peek() {
    pour();
    assert(!out_stack.empty());
    return out_stack.top();
  }

  bool empty() { return in_stack.empty() && out_stack.empty(); }
};

/**
 * Your MyQueue object will be instantiated and called as such:
 * MyQueue* obj = new MyQueue();
 * obj->push(x);
 * int param_2 = obj->pop();
 * int param_3 = obj->peek();
 * bool param_4 = obj->empty();
 */
