module Substitution
  (
   Substitution,
   comboSubs,
  ) where

import ReadPrintTerms (Term(..), isVariable)
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
comboSubs (Just s1@(_:_)) (Just s2@(_:_))   = Just (s1 ++ s2)









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
