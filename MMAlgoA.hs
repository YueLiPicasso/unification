-- Martelli and Montanari 1976 unification Algorithm A
-- I call it Unification Transform because it transforms
-- a first order term equations set (FOTEset) into its solved form
-- 23 Jun 2015 V 1

module MMAlgoA
         (
           unificationTransform,
           eliminatableVariablesExist,
           isInSolvedForm,
           isUnsolvable,
           step_a,
           step_b,
           termReduction,
           variableElimination
         ) where


import FOTEset           (FOTE, FOTEset,
                          varOccurAtLeastOnceInRestof,
                          varOccurMoreThanOnceIn)
import ReadPrintTerms    (Term(..), isVariable, occursAt)
import Substitution      (applySubs)

import Data.Tuple        (swap)
import Data.Maybe        (fromJust)
import Data.List         (find, delete)



isInSolvedForm :: FOTEset -> Bool
-- Pre-requisite:
--     None
isInSolvedForm eSet
    | eSet == []                                       = True
    | (not . all foteHasGoodForm) eSet                 = False
    | occurMoreThanOnce eSet eSet                      = False
    | otherwise                                        = True

occurMoreThanOnce :: FOTEset -> FOTEset -> Bool
-- pre-requisite:
--   In first argument, all FOTEs in good form
-- argument specifiaction:
-- first and second argument are equal in value
-- functionality:
--   check if left member (variables) of good form FOTEs occur more than once

occurMoreThanOnce [] _ = False
occurMoreThanOnce (equ:restEqu) eSet =
      equ `varOccurMoreThanOnceIn` eSet || occurMoreThanOnce restEqu eSet

--------------------------------------------------------------------------------
isUnsolvable :: FOTEset -> Bool
-- Pre-requisite:
--     isInSolvedForm returns False
isUnsolvable eSet | termReduceFail eSet             = True
                  | variableEliminationFail eSet    = True
                  | otherwise                       = False

--------------------------------------------------------------------------------
unificationTransform :: FOTEset -> Maybe FOTEset
unificationTransform eSet
   | isInSolvedForm eSet                       = Just eSet
   | isUnsolvable eSet                         = Nothing
   | (not . all foteHasGoodForm) eSet          = unificationTransform $ step_b $ step_a $ termReduction $ step_b $ step_a  eSet
   | eliminatableVariablesExist eSet eSet      = unificationTransform $ step_b $ step_a $ variableElimination $ step_b $ step_a  eSet


--step a) any t = x -> x = t where x is a vriable and t is not variable
---------------------------------------------------------------------------------
swapConditionMet :: FOTE -> Bool
swapConditionMet (left, right) = (isVariable right) && ((not . isVariable) left)

swapIf :: ((a, a)-> Bool) -> (a, a) -> (a, a)
swapIf condi tup | condi tup      = swap tup
                 | otherwise      = tup

step_a :: FOTEset -> FOTEset
-- Pre-requisite:
--     None
step_a [] = []
step_a eSet@(_:_) = map (swapIf swapConditionMet) eSet



-- step b) select any equation of the form x =x , where x is variable, erase it

step_b :: FOTEset -> FOTEset
-- Pre-requisite:
--     None
step_b = filter (not . erasable)

erasable :: FOTE -> Bool
erasable (Variable a, Variable b) | a == b   = True
                                  | otherwise = False
erasable _ = False



--step c) select any equation of form t = t' where t and t' are not variables
--if t and t' have different root function symbol return fail otherwiseapply term reduction


singleFail :: FOTE -> Bool
-- tell whether term reduction shall return failure on an equation
singleFail ((Constant a), (Constant c)) | a /= c  = True
                                        | otherwise = False

singleFail ((Function f a ts), (Constant c))
                      | f /= c || a /= 0 || ((not . null) ts)  =  True
                      | otherwise                    =  False

singleFail ((Constant c), (Function f a ts))
                      | f /= c || a /= 0 || ((not . null) ts) = True
                      | otherwise                   =  False

singleFail ((Function g b ts1), (Function f a ts2))
                      | f /= g || a /= b || ((length ts1) /= (length ts2))  =  True
                      | otherwise         =  False
singleFail _ = False

--------------------------------------------------------------------------------
termReduceFail ::  FOTEset -> Bool
-- check-of-fail for term reduction; point-free definition of function
-- True case:
--      there is any FOTE makes singleFail return True
-- False case:
--      no FOTE makes singleFail return True

termReduceFail = any singleFail

--------------------------------------------------------------------------------
reduceFOTE :: FOTE -> FOTEset
--applies to any FOTE in the FOTE set when the set doesn't make term reduction fail
reduceFOTE ((Constant _), (Constant _))            = []
reduceFOTE ((Function _ _ _), (Constant _))        = []
reduceFOTE ((Constant _), (Function _ _ _))        = []
reduceFOTE ((Function _ _ ts1), (Function _ _ ts2))  = zip ts1 ts2
reduceFOTE fote                                    = [fote]

---------------------------------------------------------------------------------
termReduction :: FOTEset -> FOTEset
--operates when check-of-fail shows not fail for term reduction
termReduction = concat . map reduceFOTE


-- step d) variable elimination: choose any equation of the form x = t where x is
-- a variable that occurs somewhere else in the set; t /= x; if x occurs in t return
-- fail othwise perform variable elimination

--------------------------------------------------------------------------------

foteHasGoodForm :: FOTE -> Bool

-- tell whether a FOTE has the form x = t where x is a variable and t is not x
-- i.e. tell whether a FOTE is good form FOTE

foteHasGoodForm (Variable l, Variable r) | l /= r       = True
                                         | otherwise    = False

foteHasGoodForm (Variable _ , _ )                       = True

foteHasGoodForm _                                       = False
---------------------------------------------------------------------------------
goodFormFOTEPassesOccursCheck :: FOTE -> Bool

-- Argument specification:
--     FOTE must has good form (x = t)
-- Functionality:
--     see whether x occurs in t

goodFormFOTEPassesOccursCheck (_ , Variable _)  =  True
goodFormFOTEPassesOccursCheck (_ , Constant _)  =  True
goodFormFOTEPassesOccursCheck (x , t) | x `occursAt` t = False
                                      | otherwise      = True
---------------------------------------------------------------------------------
variableEliminationFail :: FOTEset -> Bool
variableEliminationFail = not . all goodFormFOTEPassesOccursCheck . filter foteHasGoodForm
-- False cases:
--      No good form FOTE, or
--      there is good form FOTE(s) and all good form FOTEs pass occurs check
-- True case:
--      there is good form FOTE(s) and any of them doesn't pass occurs check

---------------------------------------------------------------------------------

eliminatableVariablesExist :: FOTEset -> FOTEset -> Bool

-- Pre-requisite:
--           variableEliminationFail False cases
-- Arguments specification:
--       First argument is the set of good form FOTEs (maybe empty)
--       Second argument is a superset of the first argument
-- Functionality:
--       this funnction tells are there any FOTE in the first argument whose variable (left member) occurs
--       more than once in the second argumennt.
-- True case:
--       First argument not empty, and
--       at least one FOTE from first argument has left member, which is a variable, occurs more than once in the second argument
-- False cases:
--       No good form FOTE, or
--       there is good form FOTE and none of them has a left member, which is a variable, occurs more than once in the second argument

eliminatableVariablesExist [] _  = False
eliminatableVariablesExist (fote:fotes) eSet =
  (fote `varOccurMoreThanOnceIn` eSet) || eliminatableVariablesExist fotes eSet
--Alternatively:
--(fote `varOccurAtLeastOnceInRestof` eSet) || eliminatableVariablesExist fotes eSet

--------------------------------------------------------------------------------

isFoteThatContainsEliminatableVarialeIn :: FOTEset -> FOTE -> Bool

-- Pre-requisite:
--       eliminatableVariablesExist True Case
-- Functionality:
--       Tell whether the FOTE whose left member which is a variable, can be
--       eliminated from the FOTEset
isFoteThatContainsEliminatableVarialeIn eSet fote = fote ` varOccurMoreThanOnceIn` eSet
                                                --  Alternatively:
                                                --  fote ` varOccurMoreThanOnceIn` eSet
---------------------------------------------------------------------------------

variableElimination :: FOTEset -> FOTEset
-- Pre-requisite :
--     eliminatableVariablesExist True Case
-- The procedure:
--     find the first good form FOTE ft that contains eliminatable variable from all good form FOTES
--     remove ft from the FOTE set but keep a copy of ft elsewhere; yielding a new FOTE set fts from where one occurance of ft has been removed
--     regard ft as substitution s_ft (swap);
--     unzip fts to get ([Term],[Term])
--     apply s_ft to both lists of terms
--     zip the resulting two lists where there is no occurance of the variable to be eliminated, yielding FOTE set fts'
--     add ft back to fts'

variableElimination eSet = eSet'
   where  goodFormFOTEs = filter foteHasGoodForm eSet
          goodFormFOTE  = find (isFoteThatContainsEliminatableVarialeIn eSet) goodFormFOTEs
          ft            = fromJust goodFormFOTE
          fts           = delete ft eSet
          s_ft          = swap ft : []
          (ts1, ts2)    = unzip fts
          ts1'          = applySubs s_ft ts1
          ts2'          = applySubs s_ft ts2
          fts'          = zip ts1' ts2'
          eSet'         = ft : fts'
