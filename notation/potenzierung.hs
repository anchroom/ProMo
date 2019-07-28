-- in Folie 3.26

-- | potenzierung

-- >> lineare Rekursion
linearePotenz :: Double -> Int -> Double
linearePotenz _ 0 = 1                     -- es wird nicht aufgerufen in diesem Zweig
linearePotenz x n = x * linearePotenz x (n-1)

-- >> endständige Rekursion
-- die Funktion ' berechnet Result = x^n * acc. This time we think from result's angel.
-- But
endstaendigePotenzierung :: Double -> Int -> Double
endstaendigePotenzierung x n = endstaendigePotenzierung' 1 n
  where
    endstaendigePotenzierung' acc 0 = acc
    endstaendigePotenzierung' acc m = endstaendigePotenzierung' (x * acc) (m-1) -- but this line seems to have brain problems, it can use x all the same

-- >> noch schnelle Potenzierung mit lineareRekursion
-- Die Idee ist x^2k = (x^k)^2 und x^(2k+1)^ = (x^k)^2 * x
-- The reason why it's faster, because it reduce numbers of acc
-- 所以会一直对指数除2， 除到成为奇数为止。
schnellePotenzierung :: Double -> Int -> Double
schnellePotenzierung _ 0 = 1
schnellePotenzierung x n
  | even n    = x_to_halfn_sq
  | otherwise = x * x_to_halfn_sq
  where
    x_to_halfn_sq = square (schnellePotenzierung x (n `div` 2))
    square y = y * y