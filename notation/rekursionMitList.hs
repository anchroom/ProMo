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
insert' :: Int -> [Int] -> [Int]
insert' x [] = [x]
insert' x (y: ys)
  | x <= y    = x: y: ys
  | otherwise = y: insert' x ys

-- | sort 运用insert的sort,就是插入排序算法
sort' :: [Int] -> [Int]
sort' [] = []
sort' (x: xs) = insert' x (sort' xs)

-- | zip 把两个数组合并为一个, 其中一个是空则结果为空。
zip' :: [a] -> [b] -> [(a, b)]
zip' [] _            = []
zip' _ []            = []
zip' (x: xs) (y: ys) = (x, y): (zip' xs ys)