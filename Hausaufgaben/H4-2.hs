-- | a) einen String in eine Liste aufeinanderfolgender
--Textabschnitte und Platzhalter zerlege
{-
data Token = Text String
           | Placeholder String
             deriving (Eq, Show)

tokenize :: String -> [Token]
tokenize [] = []
tokenize ('{':x:xs) = [ Placeholder (x:xs)]
tokenize (x: xs) = [ Text (x:xs)]
otherwise = error "TODO: Implementierung vervollstaendigen"






-- | b)
--replace :: [Token] -> [(String, String)] -> String
--replace x m = error "TODO: Implementierung vervollstaendigen"

-}
data Token = Text String
           | Placeholder String
             deriving (Eq, Show)

tokenize :: String -> [Token]
tokenize [] = []
tokenize xs = parseText "" xs

parseText :: String -> String -> [Token]
parseText text ('{':xs) = Text text : parsePlaceholder "" xs
parseText text (x:xs)   = parseText (text ++ [x]) xs
parseText text ""       = [Text text]

parsePlaceholder :: String -> String -> [Token]
parsePlaceholder name ('}':xs) = Placeholder name : parseText "" xs
parsePlaceholder name (x:xs)   = parsePlaceholder (name ++ [x]) xs
parsePlaceholder name ""       = [Text name]