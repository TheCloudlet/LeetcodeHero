// Leetcode 146. LRU Cache
//
// @tag: neetcode150
// @difficulty: midium
//
// Use doubly-linked list + hash map
// Time: O(1) for both get and put


#include <list>
#include <unordered_map>

class LRUCache {
private:
  int capacity;

  // Doubly linked list: most recent at front, least recent at back
  std::list<std::pair<int, int>> cache;

  // Hash map: key -> iterator to list node
  std::unordered_map<int, std::list<std::pair<int, int>>::iterator> lookup;

public:
  LRUCache(int cap) : capacity(cap) {}

  int get(int key) {
      auto it = lookup.find(key);
      if (it == lookup.end()) {
        return -1;
      }
      // Inefficient: Erasing and re-inserting recreates node
      // Suggestion: use cache.splice(cache.begin(), cache, it->second) instead
      auto node = *(it->second);
      cache.erase(it->second);
      cache.emplace_front(node);
      lookup[key] = cache.begin();
      return node.second;
  }

  void put(int key, int value) {
      if (lookup.count(key)) {
        cache.erase(lookup[key]);
      }

      // Insert to front
      cache.emplace_front(key, value);
      lookup[key] = cache.begin();

      if (lookup.size() > capacity) {
        //Note: Always pop from back (least recently used)
        auto rm = cache.back();
        lookup.erase(rm.first);
        cache.pop_back();
      }
  }
};

/**
 * Your LRUCache object will be instantiated and called as such:
 * LRUCache* obj = new LRUCache(capacity);
 * int param_1 = obj->get(key);
 * obj->put(key,value);
 */
