module Solver where

import QM
import Covering

import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.IntSet as IS

solve :: Int -> [Int] -> [Int] -> [Set [Bit]]
solve bits minterms dontCares =
  let table = buildTable bits (minterms ++ dontCares)
      implicants = qm table
      implicantsMap = M.fromList implicants
      -- We leave out the dontCares from our universe when computing the
      -- implicants
      solutions = optimalCoverings (IS.fromList minterms) (S.fromList (fmap fst implicants))
  in fmap (S.map (implicantsMap M.!)) solutions

