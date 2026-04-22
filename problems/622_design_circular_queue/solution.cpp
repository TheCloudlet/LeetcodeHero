// Leetcode 622. Design Circular Queue

#include <vector>

class MyCircularQueue {
 private:
  std::vector<int> v_;
  int size_;
  // [Front, rear)
  int front_;
  int end_;

  inline int getNext(int i) const { return i + 1 == size_ ? 0 : i + 1; }

  inline int getPrev(int i) const { return i == 0 ? size_ - 1 : i - 1; }

 public:
  explicit MyCircularQueue(int k) : size_(k + 1), front_(0), end_(0) {
    v_.resize(k + 1);
  }

  bool enQueue(int value) {
    if (isFull()) return false;
    v_[end_] = value;
    end_ = getNext(end_);
    return true;
  }

  bool deQueue() {
    if (isEmpty()) return false;
    front_ = getNext(front_);
    return true;
  }

  int Front() const {
    if (isEmpty()) return -1;
    return v_[front_];
  }

  int Rear() const {
    if (isEmpty()) return -1;
    return v_[getPrev(end_)];
  }

  bool isEmpty() const { return front_ == end_; }

  bool isFull() const { return getPrev(front_) == end_; }
};

/**
 * Your MyCircularQueue object will be instantiated and called as such:
 * MyCircularQueue* obj = new MyCircularQueue(k);
 * bool param_1 = obj->enQueue(value);
 * bool param_2 = obj->deQueue();
 * int param_3 = obj->Front();
 * int param_4 = obj->Rear();
 * bool param_5 = obj->isEmpty();
 * bool param_6 = obj->isFull();
 */
