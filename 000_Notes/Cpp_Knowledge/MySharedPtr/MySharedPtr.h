// Objective:
// 1. Rule of 3 / rule of 5
// 2. System design
// 3. Template

template <typename T> class MySharedPtr {
private:
  T *ptr;
  int *ref_count;

  void release() {
    if (ref_count) {
      --(*ref_count);
      if (*ref_count == 0) {
        delete ptr;
        delete ref_count;
      }
    }
  }

public:
  MySharedPtr() : ptr(nullptr), ref_count(nullptr) {}

  explicit MySharedPtr(T *p) : ptr(p), ref_count(new int(1)) {}

  // Copy constructor
  MySharedPtr(const MySharedPtr &other)
      : ptr(other.ptr), ref_count(other.ref_count) {
    if (ref_count) {
      ++(*ref_count);
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
        ++(*ref_count);
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

  // Access
  T &operator*() const { return *ptr; }

  T *operator->() const { return ptr; }

  T *get() const { return ptr; }

  int use_count() const { return ref_count ? *ref_count : 0; }
};
