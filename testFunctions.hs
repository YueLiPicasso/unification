import Test.QuickCheck
import Test.QuickCheck.Monadic
import ReadPrintTerms (Term(..), occursAt)
import FOTEset (newFOTE2FOTE, NewFOTE)
import GenerateFOTE (randomFunction, randomVariable,unifiableFOTEGen)
import MMAlgoA (unificationTransform)
import UnifyTerms (unifyTerms)


prop_MMUnifiable_FOTE_Unifiable :: NewFOTE -> Bool
prop_MMUnifiable_FOTE_Unifiable newfote = prop
   where
    fote = newFOTE2FOTE newfote
    prop = (unificationTransform [fote]) /= Nothing

prop_foteUnifiableMM :: Property
prop_foteUnifiableMM = forAll (oneof unifiableFOTEGen) prop_MMUnifiable_FOTE_Unifiable

prop_UTUnifiable_FOTE_Unifiable :: NewFOTE -> Bool
prop_UTUnifiable_FOTE_Unifiable newfote = prop
   where
    (t1, t2) = newFOTE2FOTE newfote
    prop = (unifyTerms t1 t2) /= Nothing

prop_foteUnifiableUT :: Property
prop_foteUnifiableUT = forAll (oneof unifiableFOTEGen) prop_UTUnifiable_FOTE_Unifiable



prop_noOccur :: (Term, Term) -> Bool
prop_noOccur (v, f) = not (v `occursAt` f)

prop_noOccur2 = forAll genVsFs prop_noOccur

genVsFs :: Gen (Term, Term)
genVsFs = do
  v <- randomVariable
  f <- randomFunction ('v', 100) v
  return (v, f)

prop_noOccur3 ::Gen Bool
prop_noOccur3 = do
  v <- randomVariable
  f <- randomFunction ('a', 100) v
  return $ not (v `occursAt` f)

propSameSampleSize = do
  vs <- sample' randomVariable
  fs <- sample' (randomFunction ('a', 100) (Constant ""))
  return (length vs == length fs)




propNotOccurWithPrint = monadicIO propNotOccurWithPrint2

propNotOccurWithPrint2 = do
  v <- pick randomVariable
  f <- pick (randomFunction ('a', 100) v)
  run $ print v
  run $ print f
  assert $ not (v `occursAt` f)
