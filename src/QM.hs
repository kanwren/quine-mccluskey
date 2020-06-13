{-# language BangPatterns #-}
{-# language LambdaCase #-}

module QM (
    Bit(..),
    buildTable,
    qm
  ) where

import Data.Foldable (traverse_, for_)
import Data.List (foldl', sortBy, groupBy)
import Data.Ord (comparing)

import Control.Monad.Loops (iterateUntilM)
import Control.Monad.State (modify)
import Control.Monad.Trans.State.Strict (execStateT)
import Control.Monad.Writer (tell)
import Control.Monad.Trans.Writer.Strict (Writer, runWriter)

import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

-- | A bit used as part of the representation of a product. For example, for
-- inputs "wxyz", the product "w'yz" would be represented as the list
-- '[B0, Bx, B1, B1]'.
data Bit = Bx | B0 | B1
  deriving (Eq, Show, Ord, Enum, Bounded)

intToBit :: Int -> Bit
intToBit 0 = B0
intToBit 1 = B1
intToBit _ = Bx

sortAndGroupBy :: (a -> a -> Ordering) -> [a] -> [[a]]
sortAndGroupBy cmp = groupBy (\x y -> cmp x y == EQ) . sortBy cmp

-- | Converts a number to a bit vector of the given length, padding with zeros
--   if necessary. The LSB will be at the head.
toBitVector :: Int -> Int -> [Int]
toBitVector 0   _ = []
toBitVector len n = m : toBitVector (len - 1) d
  where (d, m) = n `divMod` 2

buildTable :: Int -> [Int] -> [Map IntSet [Bit]]
buildTable bits minterms =
  let
    -- turn a minterm into a row, plus the number of set bits
    withVecInfo m =
      let vec = toBitVector bits m
      in (IS.singleton m, reverse (fmap intToBit vec), foldl' (+) 0 vec)
    -- helpers
    third    (_, _, x) = x
    firstTwo (x, y, _) = (x, y)
    -- divide into groups based on number of set bits
    groups = sortAndGroupBy (comparing third) $ fmap withVecInfo minterms
    -- discard set bit info and turn each group into a map
  in fmap (M.fromList . fmap firstTwo) groups

-- | Take two bit vectors and return their match, if it exists. For example,
--   matching '01-1' and '11-1' should return '-1-1'.
match :: [Bit] -> [Bit] -> Maybe [Bit]
match [] [] = Just []
match (x:xs) (y:ys)
  | x == y    = (x:) <$> match xs ys
  | xs == ys  = Just (Bx:xs) -- if two elements differ, the rest of the list has to be the same
  | otherwise = Nothing
  -- TODO: this also allows e.g. '-000' and '0000', even though this is normally
  -- impossible. Restrict second case to check if x and y are 0 and 1?
match _ _ = error "QM.match: lists have different lengths"

combine :: ()
  => Map IntSet [Bit]
  -> Map IntSet [Bit]
  -> Writer (Set IntSet) (Map IntSet [Bit])
combine subtable1 subtable2 = flip execStateT M.empty $ do
  -- for every pair of k/v pairs between the two maps
  for_ (M.assocs subtable1) $ \(k1, v1) -> do
    for_ (M.assocs subtable2) $ \(k2, v2) -> do
      -- if the pairs match:
      -- 1. mark both checked by adding them to state
      -- 2. add to final map if union of keys isn't already there
      for_ (match v1 v2) $ \x -> do
        tell (S.singleton k1) *> tell (S.singleton k2)
        modify (M.insert (IS.union k1 k2) x)

pass :: ()
  => [Map IntSet [Bit]]
  -> Writer [(IntSet, [Bit])] [Map IntSet [Bit]]
pass [] = pure []
pass xs = do
  let pairs = zip xs (tail xs)
  -- Combine each adjacent pair to make the next layer
  let (res, checked) = runWriter $ traverse (uncurry combine) pairs
  -- Report all minterms and their corresponding bit vectors that didn't get
  -- checked, since these are the prime implicants
  let fullMap = mconcat xs
      allMinterms = M.keysSet fullMap
  traverse_ (\s -> tell [(s, fullMap M.! s)]) $ S.toList (allMinterms S.\\ checked)
  pure $ filter (not . null) res

-- | Run the Quine-McCluskey algorithm on an input table, computing the
-- implicants that can't be reduced. For example:
--
-- λ> qm $ buildTable [0, 4, 5, 7, 8, 11, 12, 15]
-- [(fromList [4,5],[B0,B1,B0,Bx]),(fromList [5,7],[B0,B1,Bx,B1]),(fromList [7,15],[Bx,B1,B1,B1]),(fromList [11,15],[B1,Bx,B1,B1]),(fromList [0,4,8,12],[Bx,Bx,B0,B0])]
qm :: [Map IntSet [Bit]] -> [(IntSet, [Bit])]
qm = snd . runWriter . iterateUntilM null pass

