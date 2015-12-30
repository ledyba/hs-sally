{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Sally.CNF (CFml(..), removeNot, toCNF, makeAlias, toDIMACS) where

import Sally.SAT
import qualified Data.HashMap.Strict as M
import Data.HashMap.Strict ((!))
import Control.Applicative ((<$>))
import System.IO (withFile, IOMode( WriteMode ), Handle, hPutStr, hPutChar)
import Data.Hashable (Hashable, hash, hashWithSalt)
import Data.Tuple (swap)

removeNot :: Fml a -> Fml a
removeNot (Not (And a)) = Or $ fmap (removeNot . Not) a
removeNot (Not (Or a)) = And $ fmap (removeNot . Not) a
removeNot (Not (Not a)) = a
removeNot (Not (FVar a)) = Not (FVar a)
removeNot (And a) = And $ fmap removeNot a
removeNot (Or a) = Or $ fmap removeNot a
removeNot (FVar a) = FVar a

data CFml a = CNot (Var a) | CAff (Var a) deriving (Show,Eq,Ord)

instance (Hashable v) => Hashable (CFml v) where
  hashWithSalt s (CAff k) = s + hash k
  hashWithSalt s (CNot k) = s + hash k + 1309482590

toCNF' :: [Int] -> Fml a -> [[CFml a]]
toCNF' _ (And []) = []
toCNF' _ (Or []) = []
toCNF' addr (And xs) = zip [0..] xs >>= (\ (idx,it) -> toCNF' (idx:addr) it)
toCNF' addr (Or [x]) = toCNF' (0:addr) x
toCNF' addr (Or (x:y:xs))  = zip [0..] cnfs >>= appendFn
      where
        cnfs = (\(i,it) -> toCNF' (i:0:addr) it) <$> zip [1..] (x:y:xs)
        ntmps = length cnfs - 1
        tmps = fmap (\p -> TmpVar (p:addr)) [1..ntmps]
        pos = fmap CAff tmps
        neg = fmap CNot tmps
        appendFn (idx, cls) = fmap (\cl -> app idx cl 0 pos neg) cls
        app _ cl n _ _ | n >= ntmps = cl
        app idx cl (cnt::Int) (pp:lpos) (np:lneg) =
                          app idx nclause (cnt+1) lpos lneg
                          where
                           nclause = if idx < cnt then cl else (if idx == cnt then pp else np):cl
        app _ _ _ _ _ = error "????"

toCNF' _ (Not (FVar p))  = [[CNot p]]
toCNF' _ (FVar p) = [[CAff p]]
toCNF' _ (Not _) = fail "????"

toCNF :: Fml a -> [[CFml a]]
toCNF = toCNF' [1]

makeAlias' :: (Eq a,Hashable a) => (M.HashMap (Var a) Int, Int) -> [CFml a] -> (M.HashMap (Var a) Int,Int)
makeAlias' = foldl f
    where
          f (dict,ncnt) fml = if M.member key dict then (dict,ncnt) else (M.insert key ncnt dict,ncnt+1)
            where
              key = getFml fml
          getFml (CNot a) = a
          getFml (CAff a) = a

makeAlias :: (Eq a,Hashable a) => [[CFml a]] -> ([[Int]], [(Int, Var a)])
makeAlias cnf = (ints, fmap swap lists)
  where
    (dict,_) = foldl makeAlias' (M.empty,1) cnf
    ints = fmap (fmap term2int) cnf
    lists = M.toList dict
    term2int (CAff x) = dict ! x
    term2int (CNot x) = -(dict ! x)

toDIMACS' :: [[Int]] -> [(Int, a)] -> Handle -> IO ()
toDIMACS' cnf dict handle = do
  hPutStr handle "p cnf "
  hPutStr handle $ show $ length dict
  hPutChar handle ' '
  hPutStr handle $ show $ length cnf
  hPutChar handle '\n'
  printCNF cnf
  where
    printCNF [] = return ()
    printCNF (x:xs) = printClause x >> printCNF xs
    printClause [] = hPutStr handle "0\n"
    printClause (x:xs) = do
      hPutStr handle $ show x
      hPutChar handle ' '
      printClause xs

toDIMACS :: [[Int]] -> [(Int, Var a)] -> FilePath -> IO()
toDIMACS cnf dict fileName = withFile fileName WriteMode (toDIMACS' cnf dict)
