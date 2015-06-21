module Substitution
  (
   Substitution,
   comboSubs,
   applySubs,
   applyOneSubs
  ) where

import ReadPrintTerms (Term(..), isVariable, occursAt)
-------------------------------------------------------------------------------
type Substitution = [(Term, Term)]

--the second Term is expected to have "Variable" as it's value constructor
--the first Term value cannot contain the same variable as the second Term value
--when the first Term value is constructed by a "Function" value constructor.
------------------------------------------------------------------------------
comboSubs :: Maybe Substitution -> Maybe Substitution -> Maybe Substitution
--the context of this function makes the first argument be (Just _)
comboSubs _ Nothing                     = Nothing
comboSubs subs1 (Just [])               = subs1
comboSubs (Just []) subs2@(Just (_:_)) = subs2
comboSubs (Just subs1@(_:_)) (Just subs2@(_:_))   = Just (s1 ++ subs2)
      where
             tsc1 = map fst subs1
             -- tsc stands for terms of substitution component
             vsc1 = map snd subs1
             -- vsc stands for variables of substitution component
             tsc1' = applySubs subs2 tsc1
             s1 = zip tsc1' vsc1
------------------------------------------------------------------------------

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









--The following code are not used
-------------------------------------------------------------------------------
--reduceSubs :: Substitution -> Substitution
--reduceSubs [] = []
--reduceSubs s@[(_,_)] = s
--reduceSubs s@(subs1:subs2:subss)
--          | or (map isVar2VarSubs s) == False = s
--          |                     = s
--          | otherwise
--               where
--               v2vSubs = takeVar2Var s
--               combinable_check = or (map (combinable v2vSubs) s)
--               v2vSubss = filter isVar2VarSubs s


------------------------------------------------------------------------------
--isVar2VarSubs :: (Term,Term) -> Bool
--tell whether a substitution is from a variable to a variable
--only if yes then this substitutuion is possible to be combined with
--another substitution
--isVar2VarSubs (t1, t2) = (isVariable t1) && (isVariable t2)
-----------------------------------------------------------------------------
--takeVar2Var :: Substitution -> (Term, Term)
--return earliest variable-to-variable substitutuion
--context of this function provides there is at least such one in list
--takeVar2Var subss =
--      let
--      (_ , next) = break isVar2VarSubs subss
--      in head next
------------------------------------------------------------------------------

--combineSubs :: (Term,Term) -> (Term,Term) -> (Term,Term)
--try to combine two substitution: if they can't be combined
--return the second argument otherwise return the composition
--note that all second elements of substitution tuples are variable
--combineSubs (Variable varTo, Variable varFrom) subs@(termTo, Variable varTo')
--       | varTo == varTo'                        = (termTo, Variable varFrom)
--       | otherwise                              = subs
--combineSubs _ _ = undefined

--combinable :: (Term,Term) -> (Term,Term) -> Bool
--combinable (Variable varTo, Variable varFrom) subs@(termTo, Variable varTo')
--       | varTo == varTo'                        = True
--       | otherwise                              = False
--combinable _ _ = undefined

----------------------------------------------------------------
