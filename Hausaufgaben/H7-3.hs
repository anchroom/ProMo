import Data.Map (Map)
import qualified Data.Map as Map 
   hiding (fromList, fromListWith, fromListWithKey,
       fromAscList,fromAscListWith, fromAscListWithKey, fromDistinctAscList,
       fromDescList,fromDescListWith, fromDescListWithKey, fromDistinctDescList)
       -- Funktionen zur Erzeugung von Maps aus Listen sind nicht erlaubt
       -- und werden deshalb nicht importiert.

-- Die Deklaration aus dem Modul Data.Map sind hier mit Praefix "Map."
-- ansprechbar. Die Aufgabe kann mit folgenden Funktionen geloest werden.
--
-- Map.empty :: Map k a
-- Map.insert :: Ord k => k -> a -> Map k a -> Map k a
-- Map.delete :: Ord k => k -> Map k a -> Map k a
-- Map.lookup :: Ord k => k -> Map k a -> Maybe a

freq :: Ord a => [a] -> Map a Int
freq [] = Map.empty
freq (x:xs) = Map.insertWith (+) x 1 (freq xs)
