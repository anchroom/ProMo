-- | das gegebene Zeichen in den gegebenen String vor dem letzten Zeichen einfügen.
-- Wenn es kein letztes Zeichen gibt (weil der String leer ist), dann soll nichts eingefu ̈gt werden.

insertBeforeLast :: String -> Char -> String
insertBeforeLast [] _ = []
insertBeforeLast (x:[]) c = c: x: []
insertBeforeLast (x: xs) c = x : (insertBeforeLast xs c)