
-- | First Int number aus
-- wrong function
foo :: [Int] -> Int
foo [x,xs] = x

-- changed from foo
foo' :: [Int] -> Int
foo' (x:xs) = x



-- | First number in last ()
-- wrong answer
bar :: [(Int, Double)] -> Int
bar (x:xs)     = bar xs
bar ((x,y):[]) = x
bar []         =42

-- change from bar
bar' :: [(Int, Double)] -> Int
bar' []           =42
bar' ((x,y):[])   = x
bar' ((x,y):rest) = bar'(rest)



-- | Put all the third number in every list into a list
-- wrong answer
qux :: [[Int]] ->[Int]
qux []           = []
qux ([x,y,z]:xs) = z : qux xs

-- change from qux
qux' :: [[Int]] ->[Int]
qux' []           = []
qux' ((_:_:x:xs):rest) = x : qux rest
