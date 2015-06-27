module UnificationDemo where

import Test.QuickCheck
import Data.Maybe (fromJust)
import FOTEset
import MMAlgoA
import UnifyTerms
import GenerateFOTE (unifiableFOTEGen4Demo)

printFOTEset :: IO ()
printFOTEset = do
   putStrLn "\n*** Each single equation is unifiable but the whole set is not necessarily unifiable.\n"
   smps <- sample' $ frequency unifiableFOTEGen4Demo
   putStrLn $ show $ NFset smps


unificationDemo :: String -> IO ()
unificationDemo uniAlgo = do
  newfote <- generate $ frequency unifiableFOTEGen4Demo
  let fote = newFOTE2FOTE newfote
  if uniAlgo == "MM1976A"
  then do
     let solvedForm' = unificationTransform [fote]
     let solvedForm = NFset $ map fote2newFOTE $ fromJust solvedForm'
     putStrLn $ show $ UniQA_MM1976A (newfote, solvedForm)
  else putStrLn "undefined"
