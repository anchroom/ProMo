-- | a) einen String in eine Liste aufeinanderfolgender
--Textabschnitte und Platzhalter zerlege

data Token = Text String
           | Placeholder String
             deriving (Eq, Show)

tokenize :: String -> [Token]
tokenize [] = []
-- avoid the situation, when there is no Placeholder, Text""
tokenize ('{':xs) = parsePlaceholder "" xs
tokenize xs = parseText "" xs
otherwise = error "TODO: Implementierung vervollstaendigen"


parseText :: String -> String -> [Token]
-- avoid the situation, Text"" to output
parseText [] ""         = []
parseText text ('{':xs) = Text text: parsePlaceholder "" xs
parseText text (x:xs)   = parseText (text ++ [x]) xs
-- for no Placeholder
parseText text ""       = [Text text]

parsePlaceholder :: String -> String -> [Token]
parsePlaceholder name ('}':xs) = Placeholder name : parseText "" xs
parsePlaceholder name (x:xs)   = parsePlaceholder (name ++ [x]) xs


-- | b) replace all Placeholder in Token with special String


replace :: [Token] -> [(String, String)] -> String
replace [] ((original, replacement):rest) = []
replace [Text text] ((original, replacement):rest) = text
replace [Placeholder placeholder] ((original, replacement):rest) = case lookup placeholder ((original, replacement):rest) of
                                                                               Just value  -> value
                                                                               Nothing     -> placeholder
replace (token1: token2) ((original, replacement):rest) = replace [token1]((original, replacement):rest)  ++ replace token2 ((original, replacement):rest)


{-
lookupReplacement :: String -> [(String, String)] -> String
lookupReplacement placeholder ((original, replacement):rest) = case lookup placeholder ((original, replacement):rest) of
                                                                  Just value  -> value
                                                                  Nothing     -> placeholder



untokenize :: [Token] -> [(String, String)]
untokenize [] = []
untokenize [Text text] = [("Text", text)]
untokenize [Placeholder placeholder] = [("Placeholder", placeholder)]
untokenize (token1 : token2) = untokenize [token1] ++ untokenize token2

-}

