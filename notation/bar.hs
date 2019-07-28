-- in Folie 1.47
-- | different between arity function and two arities function in the same (a+1)*b
-- foo for arity function
foo :: (Int, Int) -> Int
foo (x, y) = (x + 1) * y

-- bar for abiarities function
bar :: Int -> Int -> Int
bar x y = (x + 1) * y