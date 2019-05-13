import Data.Char

-- | Make every letter's (need to change) asciicode + 2
-- i.e. According to the result of function isAlpha, When isLetter is True,
-- Small letters remain small letters, capital letters remain capital letters.
-- Exception letters:
-- >>> 'y' , 'z', 'Y' , 'Z'
garble :: String -> String
garble [] = []
garble (x : xs)
    | x == 'y'                                      = 'a' : garble xs
    | x == 'z'                                      = 'b' : garble xs
    | x == 'Y'                                      = 'A' : garble xs
    | x == 'Z'                                      = 'B' : garble xs
    | (isAsciiUpper x) || (isAsciiLower x) == True  = chr (ord(x)+2) : garble xs
    | otherwise                                     = x : garble xs



-- | Make every letter's (need to change) asciicode - 2
-- i.e. According to the result of function isAlpha, When isLetter is True,
-- Small letters remain small letters, capital letters remain capital letters.
-- Exception letters:
-- >>> 'a' , 'b', 'A' , 'B'
ungarble :: String -> String
ungarble [] = []
ungarble (y : ys)
    | y == 'a'                                      = 'y' : ungarble ys
    | y == 'b'                                      = 'z' : ungarble ys
    | y == 'A'                                      = 'Y' : ungarble ys
    | y == 'B'                                      = 'Z' : ungarble ys
    | (isAsciiUpper y) || (isAsciiLower y) == True  = chr (ord(y)-2) : ungarble ys
    | otherwise                                     = y : ungarble ys



{-
Moduls ready for extend

-- | encode letter
encode :: Char -> Char
encode x
    | x == 'y'         = 'a'
    | x == 'z'         = 'b'
    | x == 'Y'         = 'A'
    | x == 'Z'         = 'B'
    | check x == True  = chr (ord(x)+2)
    | otherwise        = x


-- | check if a letter is
-- >>> is Letter
-- True
-- >>> is not Letter
-- False
check :: Char -> Bool
check x = (isAsciiUpper x) || (isAsciiLower x)

-}


