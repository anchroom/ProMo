-- in Folie 4,24
-- | either'
-- >>> let s = Left "foo" :: Either String Int
-- >>> let n = Right 3 :: Either String Int
-- >>> either length (*2) s
-- 3
-- >>> either length (*2) n
-- 6

either' :: (a-> c) -> (b-> c) -> (Either a b) -> c
either'     f           _           (Left l)  = f l
either'     _           g           (Right r) = g r

-- | partitionEithers'
-- Partitions a list of 'Either' into two lists.
-- All the 'Left' elements are extracted, in order, to the first
-- component of the output.  Similarly the 'Right' elements are extracted
-- to the second component of the output.
partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' [] = ([], [])
partitionEithers' (Left l: xs) = (l:ls, rs)    -- let (ls, rs) = partitionEithers' xs
  where                                        -- in case x of
    (ls, rs) = partitionEithers' xs              -- Left l  -> (l:ls, rs)
partitionEithers' (Right r: xs) = (ls, r:rs)     -- Right r -> (ls, r:rs)
  where
    (ls, rs) = partitionEithers' xs


-- zum Verbessern, benutz let anstatt where, da es gibt gleiche Ausdrück in verschiedene Fälle