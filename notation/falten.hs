-- in Folie 6,20

-- | foldl'
foldl' :: (b ->a ->b) ->b ->[a] ->b
foldl'     _            acc []  = acc
foldl'     f            acc (x:xs) = foldl' f(f acc x) xs

-- | foldr'
foldr' :: (a ->b-> b) ->b ->[a] ->b
foldr'     _            acc []  = acc
foldr'     f            acc (x: xs) = f x (foldr' f acc xs)

-- | sum
sum1 = foldl (+) 0 [1..]
sum2 = foldr (+) 0 [1..]

-- | and
and1 = foldl (&&) True
and2 = foldr (&&) True

-- | length
length' = foldr (\_ n -> succ n) 0

-- | map
map' f = foldr (\x acc -> f x : acc) []