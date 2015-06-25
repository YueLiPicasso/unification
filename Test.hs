module Main where

import Data.List
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.QuickCheck
import Test.QuickCheck
import MMAlgoA
import ReadPrintTerms

instance Arbitrary Term where
  arbitrary = do
    Positive size <- arbitrary
    sizedTerm (size `mod` 1000)

sizedTerm :: Int -> Gen Term
sizedTerm n = do
  name  <-  listOf1 $ elements (['1'..'9']++['a'..'z']++['A'..'Z'])    --return "" --arbitrary
  --i     <- arbitrary
  args  <- sizedListOf sizedTerm (n - 1)
  let i = length args
  elements ([Constant name, Variable name] ++
            if n < 2 then []
                     else [Function name i args])

-- Given a size s, generates a list where the sizes of the elements sum to <= s
sizedListOf :: (Int -> Gen a) -> Int -> Gen [a]
sizedListOf g s = do
  ns <- listOf (choose (0, s))        -- Generate random points between 0 and s
  let points = sort (take s ns)       -- Sort them
      sizes  = diffsBetween 0 points  -- Get the distances between them
  mapM g (filter (> 0) sizes)         -- Use the distances as element sizes
  where diffsBetween n []     = []
        diffsBetween n (x:xs) = (x-n) : diffsBetween x xs

sizedListRespectsBound (NonNegative n) =
  forAll (sizedListOf (return :: Int -> Gen Int) n) sumBounded
  where sumBounded ns = sum ns <= n

sizedTermRespectsBound (NonNegative n) =
  forAll (sizedTerm n) termBounded
  where termBounded t = termSize t <= n + 1
        termSize (Function _ _ ts) = 1 + sum (map termSize ts)
        termSize _                 = 1

sameTermsUnify t =
  unificationTransform [(t, t)] == Just []

differentConstantsDontUnify c1 c2 = c1 /= c2 ==>
  unificationTransform [(Constant c1, Constant c2)] == Nothing

differentConstantAndFunctionDontUnify c1 f1 i l = c1 /= f1 ==>
  unificationTransform [(Constant c1, Function f1 i l)] == Nothing

functionWithArgumentsWontUnifyWithConstant n i l =
  unificationTransform [(Constant n, Function n positive l)] == Nothing
  where positive = 1 + (abs i)

main = defaultMain $ testGroup "All tests" [
--    testProperty "Can generate bounded lists" sizedListRespectsBound
--  , testProperty "Can generate bounded terms" sizedTermRespectsBound
--  , testProperty "Different Constants Don't Unify" differentConstantsDontUnify
    testProperty "Same terms unify" sameTermsUnify
--  , testProperty "Differently named constants/functions don't unify" differentConstantAndFunctionDontUnify
--  , testProperty "Function with arguments won't unify with constant" functionWithArgumentsWontUnifyWithConstant

  ]
