#include "MySharedPtr.h"
#include <iostream>

void print(const char *label, const MySharedPtr<int> &p) {
  std::cout << label << " -> ";
  std::cout << "value: ";
  if (p.get()) {
    std::cout << *p;
  } else {
    std::cout << "(null)";
  }
  std::cout << ", use_count: " << p.use_count() << "\n";
}

int main() {
  std::cout << "Creating sp...\n";
  MySharedPtr<int> sp(new int(777));
  print("sp", sp);

  std::cout << "\nCopying sp into b...\n";
  MySharedPtr<int> b = sp;
  print("sp", sp);
  print("b ", b);

  std::cout << "\nMoving b into c...\n";
  MySharedPtr<int> c = std::move(b);
  print("b ", b);
  print("c ", c);

  std::cout << "\nResetting c to new value...\n";
  c = MySharedPtr<int>(new int(-88));
  print("sp", sp);
  print("c ", c);

  std::cout << "\nMoving sp into c...\n";
  c = std::move(sp);
  print("sp", sp);
  print("c ", c);

  std::cout << "\nLeaving main()...\n";
  return 0;
}
