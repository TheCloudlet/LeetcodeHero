// Leetcode 146. LRU Cache
//
// @tag: neetcode150
// @difficulty: medium
//
// Use doubly-linked list + hash map
// Time: O(1) for both get and put

#include <iterator>
#include <list>
#include <unordered_map>

class LRUCache {
private:
  size_t capacity;

  // Doubly linked list: most recent at front, least recent at back
  std::list<std::pair<int, int>> cache_list;
  // Hash map: key -> iterator to list node
  std::unordered_map<int, std::list<std::pair<int, int>>::iterator> map;

public:
  explicit LRUCache(int capacity_) : capacity(capacity_) {}

  int get(int key) {
    if (auto it = map.find(key); it != map.end()) {
      cache_list.splice(cache_list.begin(), cache_list, it->second);
      return it->second->second;
    }
    return -1;
  }

  void put(int key, int value) {
    // In list already
    if (auto it = map.find(key); it != map.end()) {
      it->second->second = value;
      cache_list.splice(cache_list.begin(), cache_list, it->second);
      return;
    }

    // Reuse the last element if full
    if (cache_list.size() == capacity) {
      auto last_it = std::prev(cache_list.end());
      map.erase(last_it->first);

      last_it->first = key;
      last_it->second = value;

      cache_list.splice(cache_list.begin(), cache_list, last_it);

      map.emplace(key, cache_list.begin());
      return;
    }

    // Normal add
    cache_list.emplace_front(key, value);
    map.emplace(key, cache_list.begin());
  }
};

/**
 * Your LRUCache object will be instantiated and called as such:
 * LRUCache* obj = new LRUCache(capacity);
 * int param_1 = obj->get(key);
 * obj->put(key,value);
 */
