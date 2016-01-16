module Sally.Util (sortOn) where

import Data.List (sortBy)
import Data.Ord  ( comparing )

sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn f =
  map snd . sortBy (comparing fst) . map (\x -> let y = f x in y `seq` (y, x))
