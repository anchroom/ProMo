-- Aufgabe 7-4
-- The class declaration says "I'm going to define a bunch of functions now which will work for several different types". The instance declaration says "this is how these functions work for this type".
--In your specific example, class Eq says that "Eq means any type that has a function named ==", whereas the instance Eq Integer says "this is how == works for an Integer".
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

-- TODO: Definiere Monoid-Instanz so, dass folgende Defininition eine Funktion
-- ist, die ein minimales Element aus der gegebenen Liste zurückgibt, oder
-- Nothing bei der leeren Liste.
--
least :: Ord a => [a] -> Maybe a 
least = toMaybe . foldMap Minimum
