#include <iostream>
#include <memory>
#include <vector>

// Base class
class Shape {
public:
  // Add virtual keyword
  virtual void draw() const {
    std::cout << "Drawing a Generic Shape" << std::endl;
  }
  // Virtual destructor is a good practice
  virtual ~Shape() = default;
};

// Derived class
class Circle : public Shape {
public:
  // Override the base class function
  void draw() const override { std::cout << "Drawing a Circle" << std::endl; }
};

class Square : public Shape {
public:
  void draw() const override { std::cout << "Drawing a Square" << std::endl; }
};

int main() {
  std::vector<std::unique_ptr<Shape>> shapes;
  shapes.push_back(std::make_unique<Shape>());
  shapes.push_back(std::make_unique<Circle>());
  shapes.push_back(std::make_unique<Square>());

  // Call draw() through base class pointer Shape*
  for (const auto &shape : shapes) {
    shape->draw(); // << Because draw() is virtual, dynamic binding occurs here
  }

  return 0;
}