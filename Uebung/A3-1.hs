--?? was ist Subtitutionsmodell

-- | a) impies True False with Substitutionsmodell
-- neg :: Bool -> Bool
-- neg x = if x then False else True


-- My answer> \ x y -> if (not x) then False else y) True False



-- | b)multiply :: [Int] -> Int
--multiply [] = 1
--multiply (x:xs) = x * multiply xs
\((x : xs) -> x * \ xs)  (1 : 2 : 3 : [])


-- | c)(\x -> \y -> x - y) (3 + 4) (multiply [1, 2, 3])
>

