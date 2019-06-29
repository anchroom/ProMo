------------------
-- Aufgabe A8-1 --
------------------

data HTML = Text String
          | Tag String [HTML]

example :: HTML
example = Tag "html" [Tag "body" [Tag "p" [Text "Foo"], Tag "p" [Text "Bar"], Tag "p" [Text "Bla"]]]

-- |Show-Instanz fuer HTML definieren
instance Show HTML where
    show (Text s) = s
    show (Tag tag children) = "<" ++ tag ++ ">"