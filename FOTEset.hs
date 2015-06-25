module FOTEset
   (
     FOTE
   , FOTEset
   , NewFOTE (..)
   , occurMoreThanOnceIn
   , newFOTE2FOTE
   ) where

import ReadPrintTerms (Term(..), occursAt)

-- FOTEset stands for First Order Term Equations set

type FOTEset = [FOTE]
type FOTE = (Term, Term)
newtype NewFOTE  = NF FOTE

-- each equation is represented by a tuple;
-- fisrt member of the tuple is the left member of the equation
-- second member of the tuple is the right member of the equation
instance Show NewFOTE where
  show (NF (t1, t2)) = show t1 ++ " = " ++ show t2

newFOTE2FOTE :: NewFOTE -> FOTE
newFOTE2FOTE (NF fote) = fote
--------------------------------------------------------------------------------
occurMoreThanOnceIn :: Term -> FOTEset -> Bool

-- Pre-requisite:
--     None
-- Arguments specification:
--     Term is the left member of a good form FOTE, which means Term has constructor Variable
--     FOTEset is the FOTE set to be transformed
-- Functionality:
--     tells whether a variable occurs more than once in the FOTEset;

occurMoreThanOnceIn = occurMoreThanOnceInAccum 0
--------------------------------------------------------------------------------

occurMoreThanOnceInAccum :: Int -> Term -> FOTEset -> Bool

-- Int is accumulator; n > 1 returns true but n may not be the exact number by which that Term occur in FOTEset

occurMoreThanOnceInAccum n v ((lm, rm):fotes) =
    if  v `occursAt` lm  || v `occursAt` rm
    then occurMoreThanOnceInAccum (n+1) v fotes
    else occurMoreThanOnceInAccum n v fotes

occurMoreThanOnceInAccum n v [] | n > 1     = True
                                | otherwise = False
