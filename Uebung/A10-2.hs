{-# LANGUAGE InstanceSigs #-} 
-- 10. Übung Programmierung und Modellierung, SoSe19, TCS, LMU  München 
-- A10-3 Fehler Monade Vorlage 

import Control.Applicative
import Control.Monad


mySequence :: Monad m => [m a] -> m [a]
mySequence = error "TODO" 


mySequenceA :: Applicative m => [m a] -> m [a]
mySequenceA = error "TODO" 
