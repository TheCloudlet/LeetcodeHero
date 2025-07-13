// Objectives:
// 1. Practice the Rule of 3 and Rule of 5 in C++
// 2. Explore small-scale system design for resource ownership
// 3. Use templates to support generic types (T)
//
// Reference:
// - https://en.cppreference.com/w/cpp/memory/shared_ptr.html
// - https://en.cppreference.com/w/cpp/language/rule_of_three.html
// - https://en.cppreference.com/w/cpp/atomic/atomic.html

#include <memory>

template <typename T> class MySharedPtr {
private:
  T *ptr;
  std::atomic<int> *ref_count;

  void release() {
    if (ref_count && ref_count->fetch_sub(1, std::memory_order_acq_rel) == 1) {
      delete ptr;
      delete ref_count;
    }
  }

public:
  MySharedPtr() : ptr(nullptr), ref_count(nullptr) {}

  explicit MySharedPtr(T *p) : ptr(p), ref_count(new std::atomic<int>(1)) {}

  // Copy constructor
  MySharedPtr(const MySharedPtr &other)
      : ptr(other.ptr), ref_count(other.ref_count) {
    if (ref_count) {
      ref_count->fetch_add(1, std::memory_order_relaxed); // atomic++
    }
  }

  // Copy assignment
  MySharedPtr &operator=(const MySharedPtr &other) {
    if (this != &other) {
      // "this" is a pointer to the current object,
      // "&other" is the address of the source object (passed by reference).
      // If they're the same, we're assigning to ourselves — so skip.
      release();
      ptr = other.ptr;
      ref_count = other.ref_count;
      if (ref_count) {
        ref_count->fetch_add(1, std::memory_order_relaxed); // atomic++
      }
    }
    return *this;
  }

  // Move constructor
  MySharedPtr(MySharedPtr &&other) noexcept
      : ptr(other.ptr), ref_count(other.ref_count) {
    other.ptr = nullptr;
    other.ref_count = nullptr;
  }

  // Move assignment
  MySharedPtr &operator=(MySharedPtr &&other) noexcept {
    if (this != &other) {
      release();
      ptr = other.ptr;
      ref_count = other.ref_count;
      other.ptr = nullptr;
      other.ref_count = nullptr;
    }
    return *this;
  }

  ~MySharedPtr() { release(); }

  // Observers
  T &operator*() const { return *ptr; }

  T *operator->() const { return ptr; }

  T *get() const { return ptr; }

  int use_count() const { return ref_count ? ref_count->load() : 0; }
};
