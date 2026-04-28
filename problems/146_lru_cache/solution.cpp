// Leetcode 146. LRU Cache
//
// @tag: neetcode150
// @difficulty: medium

#if defined(LIST)
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

#endif

#if defined(VECTOR)
#include <unordered_map>
#include <vector>

class LRUCache {
 private:
  struct Node {
    int key;
    int value;
    int next;
    int prev;
  };

  std::vector<Node> nodes_;
  std::unordered_map<int, int> key_to_idx_;
  int front_ = -1;
  int back_ = -1;
  int free_idx_ = 0;
  int cap_;

  inline void detachLink(int i) {
    if (nodes_[i].prev != -1)
      nodes_[nodes_[i].prev].next = nodes_[i].next;
    else
      front_ = nodes_[i].next;
    if (nodes_[i].next != -1)
      nodes_[nodes_[i].next].prev = nodes_[i].prev;
    else
      back_ = nodes_[i].prev;
  }

  inline void addToFront(int i) {
    nodes_[i].next = front_;
    nodes_[i].prev = -1;
    if (front_ != -1) nodes_[front_].prev = i;
    front_ = i;
    if (back_ == -1) back_ = i;
  }

 public:
  explicit LRUCache(int capacity) : cap_(capacity) { nodes_.resize(capacity); }

  int get(int key) {
    auto it = key_to_idx_.find(key);
    if (it == key_to_idx_.end()) return -1;

    detachLink(it->second);
    addToFront(it->second);

    return nodes_[front_].value;
  }

  void put(int key, int value) {
    auto it = key_to_idx_.find(key);
    if (it != key_to_idx_.end()) {
      nodes_[it->second].value = value;
      detachLink(it->second);
      addToFront(it->second);
      return;
    }

    int reuse_idx = -1;
    if (free_idx_ < cap_) {
      reuse_idx = free_idx_++;
    } else {
      reuse_idx = back_;
      key_to_idx_.erase(nodes_[reuse_idx].key);
      detachLink(reuse_idx);
    }

    nodes_[reuse_idx].key = key;
    nodes_[reuse_idx].value = value;
    addToFront(reuse_idx);
    key_to_idx_[key] = reuse_idx;
  }
};
#endif
