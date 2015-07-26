module FOTEset
   (
     FOTE
   , FOTEset
   , NewFOTE (..)
   , NewFOTEset (..)
   , UnificationQandA_MM1976A (..)
   , varOccurAtLeastOnceInRestof
   , varOccurMoreThanOnceIn
   , newFOTE2FOTE
   , fote2newFOTE
   ) where

import ReadPrintTerms (Term(..), occursAt)
import Data.List (concat, intersperse,partition)

-- FOTEset stands for First Order Term Equations set
-- each equation is represented by a tuple;
-- fisrt member of the tuple is the left member of the equation
-- second member of the tuple is the right member of the equation


type FOTE = (Term, Term)

newtype NewFOTE  = NF FOTE

instance Show NewFOTE where
  show (NF (t1, t2)) = show t1 ++ " = " ++ show t2

newFOTE2FOTE :: NewFOTE -> FOTE
newFOTE2FOTE (NF fote) = fote

fote2newFOTE :: FOTE -> NewFOTE
fote2newFOTE = NF


type FOTEset = [FOTE]

newtype NewFOTEset = NFset [NewFOTE]

instance Show NewFOTEset where
  show (NFset [])     = "{ }"
  show (NFset newfts) = "{\n    " ++
      (concat $ intersperse " ,\n    "  $ map show newfts)
      ++"\n}"


newtype UnificationQandA_MM1976A = UniQA_MM1976A (NewFOTE, NewFOTEset)

instance Show UnificationQandA_MM1976A where
   show (UniQA_MM1976A (newfote, newfotes)) = "\nSolve Unification Problem:\n\n" ++
           show newfote ++ "\n\nSolution:\n\n" ++ show newfotes
--------------------------------------------------------------------------------


varOccurAtLeastOnceInRestof :: FOTE -> FOTEset -> Bool

-- Pre-requisite:
--     None
-- Arguments specification:
--     The left member FOTE has constructor Variable
--     FOTEset is the FOTE set to be transformed
-- Functionality:
--     tells whether a variable as the left member of fOTE passed as the first argument
--     occurs at least once in the FOTEset excluding one instance of the first argument;

varOccurAtLeastOnceInRestof fote foteSet = (varOccurCountAccum 0 fote foteSet') > 0
    where (fotes,restfotes) = partition (== fote) foteSet
          foteSet'          = (drop 1 fotes) ++ restfotes

varOccurMoreThanOnceIn :: FOTE -> FOTEset -> Bool
varOccurMoreThanOnceIn ft fts = (varOccurCountAccum 0 ft fts) > 1
--------------------------------------------------------------------------------

varOccurCountAccum :: Int -> FOTE -> FOTEset -> Int

-- Int is accumulator; n > 1 returns true
-- Functionality:
--     tells whether a variable as the left member of fOTE passed as the second argument
--     occurs more than once in the FOTEset;

varOccurCountAccum n fote ((lm, rm):fotes) =
    if  v `occursAt` lm  && v `occursAt` rm
    then varOccurCountAccum (n+2) fote fotes
    else if v `occursAt` lm  || v `occursAt` rm
    then varOccurCountAccum (n+1) fote fotes
    else varOccurCountAccum n fote fotes
  where (v,_ ) = fote

varOccurCountAccum n v [] = n
