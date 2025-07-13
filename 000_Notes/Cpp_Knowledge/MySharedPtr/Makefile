# Compiler
CXX := clang++
CXXFLAGS := -std=c++17 -Wall -Wextra -pedantic

# Target executable
TARGET := test_shared_ptr

# Source file
SRC := main.cpp

# Default rule
all: $(TARGET)

# Build target
$(TARGET): $(SRC) MySharedPtr.h
	$(CXX) $(CXXFLAGS) -o $@ $(SRC)

# Run shortcut
run: $(TARGET)
	./$(TARGET)

# Clean build artifacts
clean:
	rm -f $(TARGET)
