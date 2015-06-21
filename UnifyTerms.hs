module UnifyTerms
   (
     unifyTerms
   ) where

import Substitution
import ReadPrintTerms (Term (..), occursAt)
import Data.Maybe (fromJust)
-----------------------------------------------------------
unifyTerms :: Term -> Term -> Maybe Substitution

unifyTerms (Constant str1) (Constant str2)
        | str1 == str2    = Just []
        | otherwise       = Nothing

unifyTerms v1@(Variable str1) v2@(Variable str2)
        | str1 == str2    = Just []
        | otherwise       = Just [(v2,v1)]

unifyTerms c@(Constant _ ) v@(Variable _ ) = Just [(c,v)]

unifyTerms v@(Variable _ ) c@(Constant _ ) = Just [(c,v)]

unifyTerms (Constant c ) (Function f a _ )
            | c /= f          = Nothing
            | c == f && a > 0 = Nothing
            | c == f && a = 0 = Just []

unifyTerms (Function f a _ ) (Constant c )
            | c /= f          = Nothing
            | c == f && a > 0 = Nothing
            | c == f && a = 0 = Just []

unifyTerms f@(Function _ _ tms ) v@(Variable _ )
     | v `occursAt` f   = Nothing
     | otherwise        = Just [(f,v)]
--occurs check needed
unifyTerms v@(Variable _ ) f@(Function _ _ tms )
    | v `occursAt` f   = Nothing
    | otherwise        = Just [(f,v)]

unifyTerms (Function name1 arity1 tms1) (Function name2 arity2 tms2)
   | name1 /= name2              = Nothing
   | arity1 /= arity2            = Nothing
   | (null tms1) && (null tms2)  = Just []
   | (null tms1) && ((not . null) tms2)  = Nothing
   | (null tms2) && ((not . null) tms1)  = Nothing
   | (length tms1) /= (length tms2)      = Nothing
   | otherwise                   = unifyTermList tms1 tms2

------------------------------------------------------------
unifyTermList :: [Term] -> [Term] -> Maybe Substitution
--the context in which unifyTermList is called requires the two
-- input list non empty and of the same length
unifyTermList (t1:[]) (t2:[]) = unifyTerms t1 t2
unifyTermList (t11:tms1@(t12:t1s)) (t21:tms2@(t22:t2s))
        | Nothing == subs1                = Nothing
        | otherwise                       = subs3
        where
            subs1 = unifyTerms t11 t21
            subs1' = fromJust subs1
            subs_tms1 = applySubs subs1' tms1
            subs_tms2 = applySubs subs1' tms2
            subs2 = unifyTermList subs_tms1 subs_tms2
            subs3 = comboSubs subs1 subs2

unifyTermList _ _ = undefined
-------------------------------------------------------------------
