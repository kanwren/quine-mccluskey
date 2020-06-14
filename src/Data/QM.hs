module Data.QM (
    quineMccluskey,
    Bit(..),
    buildTable,
    primeImplicants,
  ) where

import Data.QM.Covering

import Data.Foldable (fold, traverse_, for_)
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
-- inputs @w@, @x@, @y@, and @z@, the product @not w && y && z@ would be
-- represented as the list @[B0, Bx, B1, B1]@.
data Bit = Bx | B0 | B1
  deriving (Eq, Show, Ord, Enum, Bounded)

intToBit :: Int -> Bit
intToBit 0 = B0
intToBit 1 = B1
intToBit _ = Bx

sortAndGroupBy :: (a -> a -> Ordering) -> [a] -> [[a]]
sortAndGroupBy cmp = groupBy (\x y -> cmp x y == EQ) . sortBy cmp

-- | Converts a number to a bit vector of the given length, padding with zeros
-- if necessary. The LSB will be at the head.
toBitVector :: Int -> Int -> [Int]
toBitVector 0   _ = []
toBitVector len n = m : toBitVector (len - 1) d
  where (d, m) = n `divMod` 2

-- | Given the number of inputs and a list of minterms, build the initial table
-- to run the algorithm on.
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
-- matching '01-1' and '11-1' should return '-1-1'.
match :: [Bit] -> [Bit] -> Maybe [Bit]
match [] [] = Just []
match (x:xs) (y:ys)
  | x  == y   = (x:) <$> match xs ys
  | xs == ys  = Just (Bx:xs) -- if two elements differ, the rest of the list has to be the same
  | otherwise = Nothing
match _ _ = error "Data.QM.match: lists have different lengths" -- impossible

-- | Combine two adjacent subtables
combine :: ()
  => Map IntSet [Bit]
  -> Map IntSet [Bit]
  -> Writer (Set IntSet) (Map IntSet [Bit])
combine subtable1 subtable2 = flip execStateT M.empty $ do
  for_ (M.assocs subtable1) $ \(k1, v1) -> do
    for_ (M.assocs subtable2) $ \(k2, v2) -> do
      for_ (match v1 v2) $ \x -> do
        tell (S.singleton k1) *> tell (S.singleton k2)
        modify (M.insert (IS.union k1 k2) x)

-- | A single pass of the algorithm, combining every adjacent subtable. Every
-- pass removes at least 1 from the number of subtables.
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
  let fullMap = fold xs
      allMinterms = M.keysSet fullMap
  traverse_ (\s -> tell [(s, fullMap M.! s)]) $ S.toList $ allMinterms S.\\ checked
  pure $ filter (not . null) res

-- | Find the prime implicants from an input table
primeImplicants :: [Map IntSet [Bit]] -> [(IntSet, [Bit])]
primeImplicants = snd . runWriter . iterateUntilM null pass

-- | Run the Quine-McCluskey algorithm on a set of essential and don't-care
-- minterms, producing all minimal solutions that cover the essential minterms.
--
-- The first input is the number of inputs to your truth table, and the minterms
-- are the indices of the truth table outputs corresponding to 1 and X,
-- respectively. The output is a list of solutions, each of which is a set of
-- product terms. The set of product terms is an sum of products that optimally
-- covers the minterms.
--
-- Let's say the truth table has three inputs: x, y, and z. In this case, the
-- term @[B0, B1, Bx]@ would correspond to the term @not x && y@.
quineMccluskey :: Int -> [Int] -> [Int] -> [Set [Bit]]
quineMccluskey bits minterms dontCares =
  let table = buildTable bits (minterms ++ dontCares)
      implicants = primeImplicants table
      implicantsMap = M.fromList implicants
      -- We leave out the dontCares from our universe when solving the
      -- implicant table
      solutions = optimalCoverings (IS.fromList minterms) (S.fromList (fmap fst implicants))
  in fmap (S.map (implicantsMap M.!)) solutions

