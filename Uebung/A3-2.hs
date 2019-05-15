-- |A3-2 Rekursion Implementieren Sie folgende Funktionen:
-- a) nTimes :: Int -> Double -> [Double] soll so definiert sein, dass nTimes n d zu der
-- Liste auswertet, in der n-mal die Zahl d steht. Beispiele:
-- nTimes 4 3.0 == [3.0, 3.0, 3.0, 3.0], nTimes 0 1.8 == [],
-- nTimes (-1) 1.8 == [].
{- my answer
nTimes :: Int -> Double -> [Double]
nTimes n d = if n <= 0 then [] else ((nTimes (n-1) d) : d):[]
-}
nTimes n d = if n<= 0 then [] else d : (nTimes (n-1) d)

nTimes' nd = case x of
                          0 ->[]
                          _ -> [d] ++ (nTimes' (n-1) d)

nTimesEnd n d = nTimesEndAux n d [] where
    nTimesEndAux :: Int -> Double -> [Double] ->[Double]
    nTimesEndAux n d acc = if n <=0 then acc else
        nTimesEndAux (n-1) d (d:acc)

-- b) upToSemicolon :: String -> String soll den Anfang des gegebenen Strings bis zum ersten Semikolon zuru ̈ckgeben. Beispiele:
-- upToSemicolon "Name; Vorname; Wohnohrt" == "Name",
-- upToSemicolon "von Mustermann; Max; Starnberg" == "von Mustermann", upToSemicolon "kein Semikolon" == "kein Semikolon".
{-my answer
upToSemicolon :: String -> String
uptoSemicolon (x:xs) = if (ord x == 59) then
-}
upToSemicolon [] = []
upToSemicolon (';': _) =[]
upToSemicolon (c:cs) = c:(upToSemicolon cs)



-- c)firstOccurrence :: String -> Char -> Int soll die Position des ersten Vorkom- mens des gegebenen Zeichens im gegebenen String zuru ̈ckgeben (-1 wenn das Zeichen nicht vorkommt). Beispiele:
-- firstOccurrence "Mustermann" 'M' == 0, firstOccurrence "Mustermann" 'a' == 7, firstOccurrence "Mustermann" 'z' == -1
{-my answer

firstOccurrence (x:xs) c = if x == c
-}
firstOccurrence
firstOccurrence [] _ = -1
firstOccurrence (c:cs) y | c == y = 0
firstOccurrence (c:cs) y =
    let p = firstOccurrence cs u
        in if p < 0 then -1 else 1+p