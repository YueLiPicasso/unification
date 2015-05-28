module UnifyTerms
   (
     unifyTerms
    , applySubs
   ) where

import Substitution
import ReadPrintTerms (Term (..))
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
----------------------------------------------------------------
applySubs :: Substitution -> [Term] -> [Term]
-- the context of using this function provides that termsList is not empty
applySubs [] termsList         = termsList
applySubs (sub:[]) termsList   = map (applyOneSubs sub) termsList
applySubs (sub1:s@(_:_)) termsList = applySubs s (map (applyOneSubs sub1) termsList)
----------------------------------------------------------------
applyOneSubs :: (Term,Term) -> Term -> Term
applyOneSubs (term, v1@(Variable _)) v2@(Variable _)
           | v1 `occursAt` v2       = term
           | otherwise              = v2
-- occurs check of v1 in term is not needed since this case has been
-- covered in definition of unifyTerms
applyOneSubs ( _ , Variable _) c@(Constant _) = c
applyOneSubs s@(term, Variable _) (Function n a tms) =
    Function n a (map (applyOneSubs s) tms)
applyOneSubs _ _ = undefined
----------------------------------------------------------------
comboSubs :: Maybe Substitution -> Maybe Substitution -> Maybe Substitution
--the context of this function makes the first argument be (Just _)
comboSubs _ Nothing                     = Nothing
comboSubs subs1 (Just [])               = subs1
comboSubs (Just []) subs2@(Just (s:ss)) = subs2
comboSubs (Just s1@(_:_)) (Just s2@(_:_))   = Just (reduceSubs (s1 ++ s2))
