-- Vorlage fuer H7-2

class Monoid a => Log a where
  logMsg :: String -> a

fib :: Log a => Int -> (Int, a)
fib n =
  if n < 2 then (1, logMsg ("fib " ++ show n))
    else let (n1, log1) = fib (n-1)
             (n2, log2) = fib (n-2)
             y = n1 + n2
          in (y, log1 <> log2 <> logMsg ("fib " ++ show n))

-- |a) Geeignete Log-Instanzen der obigen Typen, sodass
-- Folgendes in GHCi funktioniert:
-- *Main> fib 3 :: (Int, FullLog)
-- (3,FullLog ["fib 1","fib 0","fib 2","fib 1","fib 3"])
data FullLog = FullLog [String] deriving Show

instance Semigroup FullLog where
  FullLog a <> FullLog b = FullLog (a ++ b)

instance Monoid FullLog where
  mempty = FullLog []

instance Log FullLog where
  logMsg x = FullLog [x]

-- *Main> fib 3 :: (Int, ReverseLog)
-- (3,ReverseLog ["fib 3","fib 1","fib 2","fib 0","fib 1"])
-- instance Log ReverseLog where
data ReverseLog = ReverseLog [String] deriving Show

instance Semigroup ReverseLog where
  ReverseLog a <> ReverseLog b = ReverseLog (b ++ a)

instance Monoid ReverseLog where
  mempty = ReverseLog []

instance Log ReverseLog where
  logMsg x = ReverseLog [x]


-- *Main> fib 3 :: (Int, LastMsgLog)
-- (3,LastMsgLog (Just "fib 3"))
--instance Log LastMsgLog where
data LastMsgLog = LastMsgLog (Maybe String) deriving Show

instance Semigroup LastMsgLog where
  LastMsgLog a <> LastMsgLog b = LastMsgLog (b)

instance Monoid LastMsgLog where
  mempty = LastMsgLog (Nothing)

instance Log LastMsgLog where
  logMsg x = LastMsgLog (Just x)

-- *Main> fib 3 :: (Int, CountLog)
-- (3,CountLog 5)
--instance Log CountLog where
data CountLog = CountLog Int deriving Show

instance Semigroup CountLog where
  CountLog a <> CountLog b = CountLog (a+b)

instance Monoid CountLog where
  mempty = CountLog 0

instance Log CountLog where
  logMsg x = CountLog 1


-- |b) Geeignete Log-Instanz fuer (a, b), sodass
-- Folgendes in GHCi funktioniert:
-- >>> *Main> fib 3 :: (Int, (CountLog, LastMsgLog))
-- >>> (3,(CountLog 5,LastMsgLog (Just "fib 3")))

instance (Log a, Log b) => Log (a,b) where
  logMsg x = (logMsg x, logMsg x)
