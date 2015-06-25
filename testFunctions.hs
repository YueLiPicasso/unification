import Test.QuickCheck
import Test.QuickCheck.Monadic
import ReadPrintTerms (Term(..), occursAt)
import GenerateFOTE (randomFunction, randomVariable)

prop_noOccur (v, f) = not (v `occursAt` f)

prop_noOccur3 = forAll genVsFs prop_noOccur


prop_noOccur2 ::Gen Bool
prop_noOccur2 = do
  v <- randomVariable
  f <- randomFunction ('a', 100) v
  return $ not (v `occursAt` f)

propSameSampleSize = do
  vs <- sample' randomVariable
  fs <- sample' (randomFunction ('a', 100) (Constant ""))
  return (length vs == length fs)

propIsX c = c == 'x'

genVsFs = do
  v <- randomVariable
  f <- randomFunction ('v', 100) v
  return (v, f)

propSameSizeWithPrint = monadicIO propSameSizeWithPrint2

propSameSizeWithPrint2 = do
  v <- pick randomVariable
  f <- pick (randomFunction ('a', 100) v)
  run $ print v
  run $ print f
  assert $ not (v `occursAt` f)
