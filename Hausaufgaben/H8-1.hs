------------------
-- Aufgabe H8-1 --
------------------

data Tree a = Leaf | Node (Tree a) a (Tree a)
  deriving Show

-- | a) die Werte im Baum in aufsteigender Reihenfolge zurueckgeben
-- Binaere Suchbaeume ist schon ordentiert.
-- ich glaube, Funktion: inorder soll diese Problem aufloesen
toAscList :: Ord a => Tree a -> [a]
toAscList (Node leftSubTree node rightSubTree) = toAscList leftSubTree ++ [node] ++ toAscList rightSubTree


lookupRange :: Ord a => a -> a -> Tree a -> Bool
lookupRange low high t = any (\x -> (low <= x) && (x <= high)) t
  where any f Leaf = False
        any f (Node l x r) = any f l || any f r

