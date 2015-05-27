module Substitution
  (
   Substitution
  ) where

import ReadPrintTerms (Term)

type Substitution = [(Term, Term)]

--the second Term is expected to have "Variable" as it's value constructor
--the first Term value cannot contain the same variable as the second Term value
--when the first Term value is constructed by a "Function" value constructor.
