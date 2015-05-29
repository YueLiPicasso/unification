module ReadPrintTerms
     (
       Term (..)
     , readPrintTerms
     , readTerms
     , printTerms
     , isVariable
     ) where
--A file in which each line is a term has already been converted
--into a string, the readPrintTerms function stores the terms
--information into data type Term and convert the data back to
--string but in a tidy format, i.e. not showing the value constructors
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
 show (Constant xs) = show xs
 show (Variable xs) = show xs
 show (Function name arity xs) = (show name) ++ (show xs)
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
