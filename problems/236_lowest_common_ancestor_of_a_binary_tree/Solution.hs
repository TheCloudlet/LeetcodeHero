-- | 236. Lowest Common Ancestor of a Binary Tree
module Solution where

import Control.Applicative ((<|>))

data Tree a
  = Empty
  | Node a (Tree a) (Tree a)
  deriving (Show, Eq)

lowestCommonAncestor :: Eq a => Tree a -> a -> a -> Maybe a
lowestCommonAncestor Empty _ _ = Nothing
lowestCommonAncestor (Node val left right) p q
  | val == p || val == q = Just val
  | otherwise =
      let l = lowestCommonAncestor left p q
          r = lowestCommonAncestor right p q
       in case (l, r) of
            (Just _, Just _) -> Just val
            _ -> l <|> r
