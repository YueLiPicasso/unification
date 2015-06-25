module ReadPrintTerms
     (
       Term (..)
     , readPrintTerms
     , readTerms
     , printTerms
     , isVariable
     , isFunction
     , occursAt
     , getName
     ) where
--A file in which each line is a term has already been converted
--into a string, the readPrintTerms function stores the terms
--information into data type Term and convert the data back to
--string but in a tidy format, i.e. not showing the value constructors

import Data.List (concat, intersperse)
-----------------------------------------------------------------
data Term = Constant String
          | Variable String
          | Function String Int [Term]
       deriving (Read, Eq)

--Constant: begin with lower case English letter followed
--          by any combination of underscore and alphanumeric
--          character
--Variable: Same as Constant except begining with uppercase
--          English letter
--Function: Name of function follows the same lexical rule
--          as Constant. Int stores its arity, which is the
--          length of [Term]
-----------------------------------------------------------------
instance Show Term where
 show (Constant xs) = xs
 show (Variable xs) = xs
 show (Function name 0 []) = name
 show (Function name arity xs) = name ++ "("
      ++ (concat $ intersperse "," $ map show xs) ++ ")"
-----------------------------------------------------------------
readPrintTerms :: String -> String
readPrintTerms input =
--input is the file as a single string
  let inputLines = lines input
      compactInputLines = filter (/= "") inputLines
      listOfTerms = (map read compactInputLines) :: [Term]
      termsLines = map show listOfTerms
      output = unlines termsLines
  in output
--------------------------------------------------------------
readTerms :: String -> [Term]
readTerms input =
  -- get input file and return a list of terms as [Term]
  let inputLines = lines input
      compactInputLines = filter (/= "") inputLines
      listOfTerms = (map read compactInputLines) :: [Term]
  in listOfTerms

printTerms :: [Term] -> String
printTerms listOfTerms =
  let
      termsLines = map show listOfTerms
      output = unlines termsLines
  in output
--------------------------------------------------------------------
isVariable :: Term -> Bool
isVariable (Variable _) = True
isVariable _ = False

isFunction :: Term -> Bool
isFunction (Function _ _ _ ) = True
isFunction _ = False
----------------------------------------------------------------------
occursAt :: Term -> Term -> Bool
v@(Variable str) `occursAt` (Function _ _ tms) = or (map (occursAt v) tms)
(Variable str1 ) `occursAt` (Variable str2 )
       | str1 == str2      = True
       | otherwise         = False
(Variable _ ) `occursAt` (Constant _ ) = False
_ `occursAt` _ = undefined
---------------------------------------------------------------
getName :: Term -> String
getName (Constant cn) = cn
getName (Variable vn) = vn
getName _ = undefined
