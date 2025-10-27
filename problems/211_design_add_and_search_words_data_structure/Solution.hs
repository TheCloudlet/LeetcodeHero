-- Leetcode 211. Design Add and Search Words Data Structure

module Solution where

import Data.List (foldl') -- For idiomatic construction in main
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (maybe)

-- | A Trie is a tree-like data structure for storing a set of strings.
-- Each node represents a prefix, and a boolean marks if it's a complete word.
data Trie = Node
  { isWordEnd :: Bool,
    children :: Map Char Trie
  }
  deriving (Show)

-- | An empty Trie with no words.
emptyTrie :: Trie
emptyTrie = Node False Map.empty

-- | Inserts a word into the Trie, returning a new Trie.
insert :: String -> Trie -> Trie
insert [] (Node _ currentChildren) = Node True currentChildren
insert (char : rest) (Node isEnd currentChildren) =
  let -- Find or create the child for the current character.
      child = Map.findWithDefault emptyTrie char currentChildren
      -- Recursively insert the rest of the word into that child.
      updatedChild = insert rest child
      -- Update the children map with the modified child.
      newChildren = Map.insert char updatedChild currentChildren
   in Node isEnd newChildren

-- | Searches for a word in the Trie. The character '.' can match any letter.
search :: String -> Trie -> Bool
search [] trie = isWordEnd trie
search (char : rest) trie
  | char == '.' = any (search rest) (Map.elems $ children trie)
  | otherwise =
      let m_child = Map.lookup char (children trie)
       in -- Use `maybe` for a concise way to handle the Maybe type.
          -- If lookup fails (Nothing), return False.
          -- If it succeeds (Just child), recursively search in the child.
          maybe False (search rest) m_child

main :: IO ()
main = do
  let wordsToAdd = ["bad", "dad", "mad", "pad", "bat", ".ad", "b.."]
  -- Use a fold to idiomatically build the Trie from a list of words.
  let trie = foldl' (flip insert) emptyTrie wordsToAdd

  putStrLn $ "Searching for 'pad': " ++ show (search "pad" trie) -- True
  putStrLn $ "Searching for 'bad': " ++ show (search "bad" trie) -- True
  putStrLn $ "Searching for 'b.d': " ++ show (search "b.d" trie) -- True
  putStrLn $ "Searching for 'b..': " ++ show (search "b.." trie) -- True
  putStrLn $ "Searching for '....': " ++ show (search "...." trie) -- False
  putStrLn $ "Searching for 'foo': " ++ show (search "foo" trie) -- False
