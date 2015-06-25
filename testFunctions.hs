import Test.QuickCheck
import ReadPrintTerms (Term(..), occursAt)
import GenerateFOTE (randomFunction, randomVariable)

prop_noOccur ::  Gen Bool
prop_noOccur = do
   v <- randomVariable
   f <- randomFunction ('v', 100) v
   return $ not (v `occursAt` f)

prop_noOccur2 ::Gen Bool
prop_noOccur2 = do
  v <- randomVariable
  f <- randomFunction ('a', 100) v
  return $ not (v `occursAt` f)
