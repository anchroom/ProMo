{-# OPTIONS_GHC -Wincomplete-patterns #-}


sort2 :: (Int , Int) -> (Int , Int)
sort2 (ersteNum, zweiteNum)
    | zweiteNum < ersteNum  = (zweiteNum, ersteNum)
    | otherwise = (ersteNum, zweiteNum)


sort3 :: (Int , Int , Int) -> (Int , Int , Int)
sort3 (a, b, c)
    | (a < c)  && (c < b) = (a, c, b)
    | (b < a)  && (a < c) = (b, a, c)
    | (b < c)  && (c < a) = (b, c, a)
    | (a > b)  && (b > c) = (c, b, a)
    | (c < a)  && (a < b) = (c, a, b)
    | (a == c) && (a < b) = (a, c, b)
    | (a == c) && (a > b) = (b, a, c)
    | (a == b) && (c < a) = (c, a, b)
    | (b == c) && (b < a) = (b, c, a)
    | otherwise = (a, b, c)

sort2' :: [Int] -> [Int]
sort2' [x : xs] = if (xs < x) then xs : x : [] else x : xs : []
{-
sort3' :: [Int] -> [Int]
sort3' [_ : dritteNum] = sort2' [ersteNum] :sort2'(zweiteNum : dritteNum : [])


sortFirst3 :: [Int] -> [Int]
sortFirst3 [] = []
sortFirst3 [_] = [_]
sortFirst3 [_ : _] = sort2'(_, _)
-}
