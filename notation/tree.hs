-- in der Folie 7.7

data Tree a = Empty | Node a (Tree a) (Tree a) deriving Show

-- | definieren instance , zu ein Tree formulieren
instance (Show a) => Show (Tree a) where
  show (Empty) = "Empty"
  show (Node a Empty Empty) = '<': show a ++ ">"
  show (Node a l r) = '(': show a ++ ',': show l ++ ',': show r ++ ")"

-- | instance of Functor
instance Functor Tree where
  fmap _ (Empty) = Empty
  fmap f (Node a l r) = Node (f $ a) (f <$> l) (f <$> r)


-- | height funktion,  Höhe von Bäume zu definieren
height :: Tree a -> Integer
height (empty) = 0
height (Node _ l r) = 1 + max (height l) (height r)

-- | map for tree
mapTree :: (a -> b) -> (Tree a) -> (Tree b)
mapTree _ Empty = Empty
mapTree f (Node a l r) = Node (f a) (mapTree f l) (mapTree f r)