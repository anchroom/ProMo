--in Folie 3.28-31

-- | length, to count how many elements a list have
-- >> lineareRekursiv
length' :: [a] -> Int
length' [] = 0
length' (x: xs) = 1 + length xs

-- | reverse
reverse' :: [a] -> [a]
reverse' [] = []
reverse' [x] = [x]
reverse' (x: xs) = reverse xs ++ [x]

-- | insert 按照顺序插入
-- how to make type constraint
insert' :: Eq => a -> [a] -> [a]
insert' x [] = [x]
insert' x [y: ys]
  | x <= y  = x: (insert' y ys)
  | y < x   = y: (insert' x ys)

-- | sort 运用insert的sort,就是插入排序算法
sort' :: Eq => [a] -> [a]
sort' [] = []
sort' (x: xs) = insert' x (sort xs)

-- | zip 把两个数组合并为一个
zip' :: [a] -> [b] -> [(a, b)]
zip' [] []           = []
zip' [] (y: ys)      = ([],y): (zip' [] ys)
zip' (x: xs) []      = (x,[]): (zip' xs [])
zip' (x: xs) (y: ys) = (x, y): (zip' xs ys)