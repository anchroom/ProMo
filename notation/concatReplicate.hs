-- in Folie 2.55
-- | concat n times [a] with [a]

concatReplicate :: Int -> [a] -> [a]
concatReplicate _ [] = []
concatReplicate n (x:xs)
  | n <= 0 = []
  | otherwise = concatReplicateAux n x xs
    where
      concatReplicateAux 0 _ [] = []
      concatReplicateAux 0 _ (h:t) = concatReplicateAux n h t      -- whnn concat x is over , dann nächst
      concatReplicateAux c h t = h : concatReplicateAux (c-1) h t
