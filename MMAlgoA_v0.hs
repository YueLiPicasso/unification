--Martelli and Montanari 1976 unification Algorithm A
--I call it Unification Transform because it is transforms
--a first order term equations set (FOTEset) into its solved form


module MMAlgoA
         (
           --unifiTransform
           swapConditionMet
         ) where

import FOTEset (FOTE, FOTEset)
import ReadPrintTerms (Term(..), isVariable)

import Data.Tuple (swap)





--step a) any t = x -> x = t where x is a vriable and t is not variable

swapConditionMet :: FOTE -> Bool
swapConditionMet (left, right) = (isVariable right) && ((not . isVariable) left)

swapIf :: ((a, a)-> Bool) -> (a, a) -> (a, a)
swapIf condi tup | condi tup      = swap tup
                 | otherwise      = tup

step_a :: FOTEset -> FOTEset
step_a [] = []
step_a eSet@(_:_) = map (swapIf swapConditionMet) eSet






-- step b) select any equation of the form x =x , where x is variable, erase it

step_b :: FOTEset -> FOTEset
step_b = filter (not . erase)
       where erase (Variable a, Variable b) | a == b   = True
                                            | otherwise = False
             erase _ = False





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







termReduceFail ::  FOTEset -> Bool
-- check-of-fail for term reduction; point-free definition of function

termReduceFail = any singleFail





reduceable :: FOTE -> Bool
--operates when check-of-fail shows not fail for term reduction

reduceable ((Constant a), (Constant c)) | a == c  = True
                                        | otherwise = False

reduceable ((Function f a ts), (Constant c))
                      | f == c && a == 0 && (null ts)  =  True
                      | otherwise                    =  False

reduceable ((Constant c), (Function f a ts))
                      | f == c && a == 0 && (null ts) =  True
                      | otherwise                   =  False

reduceable ((Function g b ts1), (Function f a ts2))
                      | f == g && a == b && ((length ts1) == (length ts2))  =  True
                      | otherwise         =  False
reduceable _ = False





reduceFOTE :: FOTE -> FOTEset
-- operates on the single equation that is judged to be reduceable by the function "reducaeable"
reduceFOTE ((Constant _), (Constant _))            = []
reduceFOTE ((Function _ _ _), (Constant _))        = []
reduceFOTE ((Constant _), (Function _ _ _))        = []
reduceFOTE ((Function _ _ ts1), (Function _ _ ts2))  = zip ts1 ts2





reduceAnyFOTE :: FOTE -> FOTEset
--operates when check-of-fail shows not fail for term reduction
reduceAnyFOTE fote | reduceable fote  = reduceFOTE fote
                   | otherwise        = fote : []





termReduction :: FOTEset -> FOTEset
--operates when check-of-fail shows not fail for term reduction
termReduction = concat . map reduceAnyFOTE
