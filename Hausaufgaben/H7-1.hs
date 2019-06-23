-- Vorlage fuer H7-1
--
-- Verwenden Sie in dieser Aufgabe 
-- WEDER REKURSION NOCH LIST-COMPREHENSIONS.
--
-- Erlaubt sind alle Funktionen der Prelude, 
-- insbesondere map, filter, zip, concat, reverse, foldr
-- 
-- Zur Definition von Listen können Sie Ausdrücke
-- der Form [n..m] verwenden. NICHT verwenden duerfen
-- Sie List-Comprehensions der Form [e | ...].

-- | Binärdarstellung einer Booleschen Werten
binaryToInteger :: [Bool] -> Integer
binaryToInteger xs = sum $ zipWith (*) (reverse $ map (\x -> if x== True then 1 else 0) xs) (map (2^) [0,1..])


upto :: Eq a => (a -> Bool) -> [a] -> [a]
upto f xs = fst $ break f xs

-- x -> the element we are checking
-- result means the String we have checked
-- !!! It's important to understand type
removeAdjacentDuplicates :: Eq a => [a] -> [a]
removeAdjacentDuplicates = foldr (\x result -> if ( [x] == (take 1 result)) then result else (x:result)) []
--removeAdjacentDuplicates (x: xs) = foldr (if ( x == (head xs)) then xs else (x: xs)) []