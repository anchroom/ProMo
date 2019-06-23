-- Aufgabe 7-4

-- a) 
--
data Sum = Sum Int
           deriving Show

getSum :: Sum -> Int
getSum (Sum i) = i

instance Semigroup Sum where
  Sum i <> Sum j = Sum (i + j)

instance Monoid Sum where
  mempty = Sum 0

mystery :: [a] -> Int
mystery = getSum . foldMap (\_ -> Sum 1)


-- b)

data Minimum a = None | Minimum a
                 deriving Show

toMaybe :: Minimum a -> Maybe a
toMaybe None = Nothing
toMaybe (Minimum i) = Just i

-- Definiere Monoid-Instanz so, dass folgende Defininition eine Funktion
-- ist, die ein minimales Element aus der gegebenen Liste zurückgibt, oder
-- Nothing bei der leeren Liste.
--
least :: Ord a => [a] -> Maybe a 
least = toMaybe . foldMap Minimum
