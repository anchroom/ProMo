------------------
-- Aufgabe A8-3 --
------------------

data Tree a = Leaf | Node (Tree a) a (Tree a)
  deriving Show



isSearchTree :: Ord a => Tree a -> Bool
isSearchTree = error "TODO"
