module Sally.SAT (Fml(..),Var(..), var) where

import Data.List (groupBy)
import Control.Applicative ((<$>))
import Data.Hashable (Hashable, hash, hashWithSalt)
import qualified Control.Arrow as CA

--------------------------------------------------------------------------------

data Var a = Var a | TmpVar [Int] deriving (Show, Read, Eq, Ord)
data Fml a = And [Fml a] | Or [Fml a] | Not (Fml a) | FVar (Var a) deriving (Show, Read, Eq)

instance (Hashable v) => Hashable (Var v) where
  hashWithSalt s (Var k) = s + hash k
  hashWithSalt s (TmpVar k) = s + hash k + 5432968

var :: a -> Fml a
var x = FVar (Var x)
