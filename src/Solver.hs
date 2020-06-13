module Solver where

import QM
import Covering

import qualified Data.Map.Strict as M

solve :: Int -> [Int] -> [[[Bit]]]
solve bits minterms =
  let table = buildTable bits minterms
      implicants = qm table
      implicantsMap = M.fromList implicants
      solutions = optimalCoverings (fmap fst implicants)
  in fmap (fmap (implicantsMap M.!)) solutions

