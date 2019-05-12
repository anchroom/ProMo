{-# OPTIONS_GHC -Wincomplete-patterns #-}


sort2 :: (Int , Int) -> (Int , Int)
sort2 (ersteNum, zweiteNum)
    | zweiteNum < ersteNum  = (zweiteNum, ersteNum)
    | otherwise = (ersteNum, zweiteNum)


sort3 :: (Int , Int , Int) -> (Int , Int , Int)
sort3 (a, b, c)
    | (a <= c)  && (c <= b) = (a, c, b)
    | (b <= a)  && (a <= c) = (b, a, c)
    | (b <= c)  && (c <= a) = (b, c, a)
    | (a >= b)  && (b >= c) = (c, b, a)
    | (c <= a)  && (a <= b) = (c, a, b)
    | otherwise = (a, b, c)

first3Num :: [Int] -> [Int]
first3Num (x: y: z: _) = [x, y, z]

sortFirst3 :: [Int] -> [Int]
sortFirst3 [] = []
sortFirst3 [a] = [a]
sortFirst3 [a , b] = if (b < a) then [b , a ] else [ a , b ]
sortFirst3 [a, b, c]
    | (a <= c)  && (c <= b) = [a, c, b]
    | (b <= a)  && (a <= c) = [b, a, c]
    | (b <= c)  && (c <= a) = [b, c, a]
    | (a >= b)  && (b >= c) = [c, b, a]
    | (c <= a)  && (a <= b) = [c, a, b]
    | otherwise = [a, b, c]
sortFirst3 (a: b: c: xs) = sortFirst3 [a, b, c] ++ [y | y <- xs]