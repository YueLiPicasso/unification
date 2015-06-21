--Martelli and Montanari 1976 unification Algorithm A
--I call it Unification Transform because it is transforms
--a first order term equations set (FOTEset) into its solved form


module MMAlgoA
         (
           --unifiTransform
           swapConditionMet
         ) where





import FOTEset (FOTE, FOTEset)
import ReadPrintTerms (Term(..), isVariable, occursAt)

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












reduceFOTE :: FOTE -> FOTEset
--applies to any FOTE in the FOTE set when the set doesn't make term reduction fail
reduceFOTE ((Constant _), (Constant _))            = []
reduceFOTE ((Function _ _ _), (Constant _))        = []
reduceFOTE ((Constant _), (Function _ _ _))        = []
reduceFOTE ((Function _ _ ts1), (Function _ _ ts2))  = zip ts1 ts2
reduceFOTE fote                                    = [fote]












termReduction :: FOTEset -> FOTEset
--operates when check-of-fail shows not fail for term reduction
termReduction = concat . map reduceFOTE









-- step d) variable elimination: choose any equation of the form x = t where x is
-- a variable that occurs somewhere else in the set; t /= x; if x occurs in t return
-- fail othwise perform variable elimination



chooseFOTE4VarElimOne :: FOTE -> Bool

--tell whether a FOTE has the form x = t where x is a variable and t is not x

chooseFOTE4VarElimOne (Variable l, Variable r) | l /= r       = True
                                               | otherwise    = False

chooseFOTE4VarElimOne (Variable _ , _ )                       = True

chooseFOTE4VarElimOne _                                       = False






chooseFOTE4VarElimTwo :: FOTE -> Bool

-- used after chooseFOTE4VarElimOne, which already made sure the FOTE has
-- form x = t and t is not x (i.e. t is a different variable, a constant or a function);
-- see whether x occur in t

chooseFOTE4VarElimTwo (_ , Variable _)  =  True
chooseFOTE4VarElimTwo (_ , Constant _)  =  True
chooseFOTE4VarElimTwo (x , t) | x `occursAt` t = False
                              | otherwise      = True

-- chooseFOTE4VarElimTwo (x , t) =
--        case t of
--        Variable _ -> True
--        Constant _ -> True
--        _          -> if x `occursAt` t
--                      then False
--                      else True







chooseFOTE4VarElimThree :: FOTE -> FOTEset -> Bool

-- used after chooseFOTE4VarElimOne&Two, when a FOTE is determined to be of the form
-- x = t where x is a variable, t is a different variable, a constant or a function and
-- x doesn't occur in t.
-- this funnction check does x occur in other FOTEs of the FOTE set.

    
