module FOTEset
   (
     FOTE
   , FOTEset
   ) where

import ReadPrintTerms (Term(..), occursAt)

-- FOTEset stands for First Order Term Equations set

type FOTEset = [FOTE]
type FOTE = (Term, Term)

-- each equation is represented by a tuple;
-- fisrt member of the tuple is the left member of the equation
-- second member of the tuple is the right member of the equation
