-- Vorlage fuer Aufgabe H5-3

data Agent = Player String Double
  | AI Int
  | Spectator String

display :: Agent -> String
display (Player name _) = name
display (AI strength) = "KI(" ++ show strength ++ ")"
display (Spectator name) = "{" ++ name ++ "}"