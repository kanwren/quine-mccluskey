module Covering (
    optimalCoverings
  ) where

import Data.Foldable (fold)
import Data.Maybe (mapMaybe, listToMaybe, fromMaybe)

import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import Data.Set (Set)
import qualified Data.Set as S

-- | Compute the sets that are the only provider of certain elements, and so
--   need to be present in a set cover. This is equivalent to finding the
--   essential prime implicants.
essentialSets :: IntSet -> Set IntSet -> Set IntSet
essentialSets toCover pis = S.fromList $ mapMaybe only $ fmap containing $ IS.toList toCover
  where
    containing x = filter (IS.member x) $ S.toList pis

    only [x] = Just x
    only _   = Nothing

-- | Given a list of elements, compute all subsets of length 0, of length 1, and
--   so on.
subsets :: [a] -> [[[a]]]
subsets elems = fmap (go elems len) [0..len]
  where
    len = length elems

    go _      _    0 = [[]]
    go []     _    _ = []
    go (x:xs) left n
      | left <= n = [x:xs] -- < should never actually happen
      | otherwise =
          let taken = fmap (x:) $ go xs (left - 1) (n - 1)
              notTaken = go xs (left - 1) n
          in taken ++ notTaken

-- | Find all minimal covering sets for the first set using sets from the second
--   set. This is equivalent to finding the minimal prime implicants that cover
--   all of the minterms.
optimalCoverings :: IntSet -> Set IntSet -> [Set IntSet]
optimalCoverings toCover pis = fmap (S.union essentials) (fromMaybe [] minimalNonEssentials)
  where
    essentials = essentialSets toCover pis
    rest = S.toList (pis S.\\ essentials)
    -- The elements we still have to cover with implicants from rest
    uncoveredElems = toCover IS.\\ fold essentials

    -- Check if a set of subsets covers uncoveredElems
    isCovering ss = uncoveredElems `IS.isSubsetOf` fold ss
    -- All minimal lists of subsets that can cover the remaining elements
    minimalNonEssentials = listToMaybe
      $ filter (not . null)
      $ fmap (filter isCovering . fmap S.fromList)
      $ subsets rest

