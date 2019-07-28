{-# LANGUAGE InstanceSigs #-} 
-- 10. Übung Programmierung und Modellierung, SoSe19, TCS, LMU  München 
-- A10-3 Fehler Monade Vorlage 

import Control.Applicative
import Control.Monad

data Problem a b = Ergebnis b | Fehler a 
  deriving (Show, Eq)  


-- TODO: instance Monad für Problem definieren!
{-instance Monod a => Problem a b where
  return x = Pure x
  Fehler a >>= f = Fehler a
  Ergebnis b >>= f =
  Ergebnis e1 <*> Ergebnis e2 = Ergebnis (e1 <*> e2)
-}

instance Functor (Problem a) where
  fmap f (Ergebnis e) =