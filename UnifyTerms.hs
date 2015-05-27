module UnifyTerms
   (
     unifyTerms
   ) where

import Substitution
import ReadPrintTerms (Term (..))
-----------------------------------------------------------
unifyTerms :: Term -> Term -> Maybe Substitution

unifyTerms (Constant str1) (Constant str2)
        | str1 == str2    = Just []
        | otherwise       = Nothing

unifyTerms v1@(Variable str1) v2@(Variable str2)
        | str1 == str2    = Just []
        | otherwise       = Just [(v1,v2)]

unifyTerms c@(Constant _ ) v@(Variable _ ) = Just [(c,v)]

unifyTerms v@(Variable _ ) c@(Constant _ ) = Just [(c,v)]

unifyTerms (Constant _ ) (Function _ _ _ ) = Nothing

unifyTerms (Function _ _ _ ) (Constant _ ) = Nothing

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
occursAt :: Term -> Term -> Bool
v@(Variable str) `occursAt` (Function _ _ tms) = or (map (occursAt v) tms)
(Variable str1 ) `occursAt` (Variable str2 )
       | str1 == str2      = True
       | otherwise         = False
(Variable _ ) `occursAt` (Constant _ ) = False
_ `occursAt` _ = undefined
---------------------------------------------------------------
unifyTermList :: [Term] -> [Term] -> Maybe Substitution
--the context in which unifyTermList is called requires the two
-- input list non empty and of the same length
unifyTermList (t1:[]) (t2:[]) = unifyTerms t1 t2
unifyTermList _ _ = undefined
