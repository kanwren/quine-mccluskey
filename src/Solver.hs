module Solver where

import QM
import Covering

import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S

solve :: Int -> [Int] -> [Set [Bit]]
solve bits minterms =
  let table = buildTable bits minterms
      implicants = qm table
      implicantsMap = M.fromList implicants
      solutions = optimalCoverings $ S.fromList $ fmap fst implicants
  in fmap (S.map (implicantsMap M.!)) solutions

