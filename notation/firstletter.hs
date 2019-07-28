-- in Folie 2.41
-- | for "as" pattern learning, to generate a sentence " The first letter of is "

firstletter :: String -> String
firstletter as@ (x: _) = "The first letter of " ++ as ++ " is " ++ [x]