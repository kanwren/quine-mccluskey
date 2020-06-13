module PI (
    solvePI
  ) where

import Data.Foldable (fold)
import Data.Maybe (mapMaybe, listToMaybe, fromMaybe)

import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import Data.Set (Set)
import qualified Data.Set as S

-- | Compute the essential prime implicants
essentialPIs :: [IntSet] -> Set IntSet
essentialPIs pis = S.fromList $ mapMaybe only $ fmap containing $ IS.toList allElems
  where
    allElems = mconcat pis
    containing x = filter (IS.member x) pis

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

-- | Solve to find all minimal prime implicants that cover all of the minterms.
--   This is equivalent to finding all minimal covering sets.
solvePI :: [IntSet] -> [[IntSet]]
solvePI pis = fmap (essentialsList ++) (fromMaybe [] minimalNonEssentials)
  where
    allElems = fold pis

    essentials = essentialPIs pis
    essentialsList = S.toList essentials
    rest = S.toList (S.fromList pis S.\\ essentials)
    -- The elements we still have to cover with implicants from rest
    uncoveredElems = allElems IS.\\ fold essentials

    -- Check if a set of subsets covers uncoveredElems
    isCovering ss = fold ss == uncoveredElems
    -- All minimal lists of subsets that can cover the remaining elements
    minimalNonEssentials = listToMaybe $ filter (not . null) $ fmap (filter isCovering) $ subsets rest

