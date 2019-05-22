-- | Lauflaengen-Kodierung und Dekodierung implementiert
{- mein Antwort

-- Function for count same char
takeSame :: Char -> String -> (Int, String)
takeSame c [] = (0, [])
takeSame c (x:xs:[]) =

runLengthEncode :: String -> [(Char, Int)]
runLengthEncode []
runLengthEncode (x:xs) =




--
runLengthDecode :: [(Char, Int)] -> String
-}


--Loesung
runLengthEncode :: String -> [(Char, Int)]
runLengthEncode xs = runLengthEncodeAux xs [] where
        runLengthEncodeAux [] acc = acc
        runLengthEncodeAux (x:xs) [] = runLengthEncodeAux xs [(x,1)]
        runLengthEncodeAux (x:xs) acc@((c,i):r)
                | x== c = runLengthEncodeAux xs ((c, i+1):r)
                | otherwise = runLengthEncodeAux ((x,1): acc)

runLengthDecode :: [(Char, Int)] -> String
runLengthDecode [] = ""
runLengthDecode ((c, i): r)
            | i > 1 = [c ] ++ (runLengthDecode ((c, i-1):r)
            | otherwise = [c ] ++ (runLengthDecode r)