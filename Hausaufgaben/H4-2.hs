-- | a) einen String in eine Liste aufeinanderfolgender
--Textabschnitte und Platzhalter zerlege
data Token = Text String
           | Placeholder String
             deriving (Eq, Show)

tokenize :: String -> [Token]
tokenize [] = []
tokenize [t] = [ Text (t:[])]
tokenize ("{": (x: xs): '}':[]) = [ Placeholder (x:xs)]
otherwise = error "TODO: Implementierung vervollstaendigen"






-- | b)
--replace :: [Token] -> [(String, String)] -> String
--replace x m = error "TODO: Implementierung vervollstaendigen"