-- 10. Übung Programmierung und Modellierung, SoSe19, TCS, LMU  München 
-- Vorlage zur H10-2

-- HINWEISE:

-- * Es folgt zuerst die Lösung der H7-2, auf der wir aufbauen.
--   Neuer Code folgt erst am Ende dieser Datei.
--
-- * Wer eine aeltere Version von GHC als 8.4.x verwendet,
-- muss noch folgende Definition hinzunehmen, damit das fib-Beispiel
-- funktioniert. 
--
-- (<>) :: Monoid a => a -> a -> a
-- (<>) = mappend
--
-- (Grund: In aktuellen GHC-Versionen ist die Verknuepfungsfunktion "<>"
-- eines Monoids von der Oberklasse Semigroup geerbt. In aelteren
-- Versionen hatte die Verknuepfungsfunktion den Namen "mappend" und
-- war in der Klasse Monoid selbst definiert. Fuer das fib-Beispiel
-- muss man dann noch explizit sagen, dass "<>" die Verknuepfungsfunktion
-- des Monoids sein soll, also "mappend".)

import Data.Semigroup

class (Semigroup a, Monoid a) => Log a where
  logMsg :: String -> a
  
fib :: (Log a) => Int -> (Int, a)
fib n =
  if n < 2 then (1, logMsg ("fib " ++ show n))
    else let (n1, log1) = fib (n-1)
             (n2, log2) = fib (n-2)
             y = n1 + n2
         in (y, log1 <> log2 <> logMsg ("fib " ++ show n))

data FullLog = FullLog [String] deriving Show
data ReverseLog = ReverseLog [String] deriving Show
data LastMsgLog = LastMsgLog (Maybe String) deriving Show
data CountLog = CountLog Int deriving Show



-- a)
-- TODO: Geeignete Log-Instanzen der obigen Typen, sodass 
-- Folgendes in GHCi funktioniert:
-- *Main> fib 3 :: (Int, FullLog)
-- (3,FullLog ["fib 1","fib 0","fib 2","fib 1","fib 3"])
-- *Main> fib 3 :: (Int, ReverseLog)
-- (3,ReverseLog ["fib 3","fib 1","fib 2","fib 0","fib 1"])
-- *Main> fib 3 :: (Int, LastMsgLog)
-- (3,LastMsgLog (Just "fib 3"))
-- *Main> fib 3 :: (Int, CountLog)
-- (3,CountLog 5)
--
--



instance Log FullLog where
  logMsg m = FullLog [ m ]

instance Semigroup FullLog where
  ( FullLog x ) <> ( FullLog y ) = FullLog ( x <> y )

instance Monoid FullLog where
  mempty = FullLog []
  mappend = (<>)
  
instance Log ReverseLog where
  logMsg m = ReverseLog [ m ]
  
instance Semigroup ReverseLog where
  ( ReverseLog x ) <> ( ReverseLog y ) = ReverseLog ( y <> x )
  
instance Monoid ReverseLog where
  mempty = ReverseLog []
  mappend = (<>) 

instance Log LastMsgLog where
  logMsg m = LastMsgLog ( Just m )
  
instance Semigroup LastMsgLog where
  ( LastMsgLog _ ) <> ( LastMsgLog ( Just m ) ) = LastMsgLog ( Just m )
  ( LastMsgLog x ) <> ( LastMsgLog Nothing ) = LastMsgLog x
  
instance Monoid LastMsgLog where
  mempty = LastMsgLog Nothing
  mappend = (<>) 
  
instance Log CountLog where
  logMsg m = CountLog 1
  
instance Semigroup CountLog where
  ( CountLog x ) <> ( CountLog y ) = CountLog ( x + y )
  
instance Monoid CountLog where
  mempty = CountLog 0
  mappend = (<>) 
-- b)
-- TODO: Geeignete Log-Instanz fuer (a, b), sodass 
-- Folgendes in GHCi funktioniert:
-- *Main> fib 3 :: (Int, (CountLog, LastMsgLog))
-- (3,(CountLog 5,LastMsgLog (Just "fib 3")))

instance (Log a,Log b) => Log(a,b) where
  logMsg f = (logMsg f,logMsg f)

  
-----------------------------
-- Neues für Aufgabe H10-2


fib' :: (Log a) => Int -> Logger a Int 
fib' n 
  | n < 2 = do
              logMsgM ("fib' " ++ show n)
              return 1
  | otherwise = do 
      n1 <- fib' (n-1)
      n2 <- fib' (n-2)
      logMsgM ("fib' " ++ show n)
      return $ n1 + n2
  

newtype Logger l a = Logger (a,l)
  deriving Show

logMsgM :: Log l => String -> Logger l ()
logMsgM s = Logger ((), logMsg s)

instance (Log l) => Functor (Logger l) where 
  fmap = error "TODO" -- durch was Sinnvolles ersetzen

instance (Log l) => Applicative (Logger l) where 
  pure  = error "TODO" -- durch was Sinnvolles ersetzen
  (<*>) = error "TODO" -- durch was Sinnvolles ersetzen
  
instance (Log l) => Monad (Logger l) where 
  return = error "TODO" -- durch was Sinnvolles ersetzen
  (>>=)  = error "TODO" -- durch was Sinnvolles ersetzen
  (>>)   = error "TODO" -- durch was Sinnvolles ersetzen
  
