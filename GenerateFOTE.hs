module GenerateFOTE
(
  randomName
, sizedTerm
, randomConstant
, randomVariable
, randomFunction
, unifiableFOTEGen
, unifiableFOTEGen4Demo
) where

-- generate unifiable FOTEs

import Data.List
import Data.Maybe (fromJust)
import Test.QuickCheck
import MMAlgoA
import ReadPrintTerms(Term(..), getName, isVariable)
import FOTEset (NewFOTE(..), newFOTE2FOTE)
import UnifyTerms (unifyTermList, unifyTerms)
import Substitution (applySubs)

instance Arbitrary Term where
  arbitrary = do
    Positive size <- arbitrary
    sizedTerm (size `mod` 1000)

randomName :: Char -> Gen String
-- generate random two letter length function or Variable name
-- 'f' for generating function name, other char for variable name
randomName c = do
   if c == 'f' then do fnameHead <- elements ['a'..'z']
                       fnameTail <- elements (['0'..'9']++['a'..'z'])
                       return (fnameHead : fnameTail : [])
               else do vnameHead <- elements ['A'..'Z']
                       vnameTail <- elements (['0'..'9']++['a'..'z'])
                       return (vnameHead : vnameTail: [])


randomConstant :: Gen Term
randomConstant = do
       fname  <-  randomName 'f'
       return (Constant fname)

randomVariable :: Gen Term
randomVariable = do
        vname  <- randomName 'v'
        return (Variable vname)


vUnifiableFOTEv :: Gen NewFOTE
-- Variable symbol = Variable Symbol (same name)
vUnifiableFOTEv = do
      v1 <- randomVariable
      return $ NF (v1, v1)


vUnifiableFOTEf :: Gen NewFOTE
--group1 {
-- Variable Symbol = Function or
-- Function = Variable Symbol or
-- }
--group2 {
-- Variable symbol = Constant Symbol or
-- Constant symbol = Variable Symbol or
-- Variable symbol = Variable Symbol (different names)
-- }
-- chance ratio: group 1 : 2 = 7 : 10
-- equal chance within group
vUnifiableFOTEf = do
      v <- randomVariable
      s <- choose (-8 , 8)
      f <- randomFunction ('v', s ) v
      n <- choose (1::Int, 2::Int)
      if n == 1 then return $ NF (v, f)
                else return $ NF (f, v)

cUnifiableFOTEf :: Gen NewFOTE
-- Constant symbol = Constant Symbol
-- Function = Constant Symbol
-- Constant symbol = Function
-- Function () = Function ()
-- equal chance
cUnifiableFOTEf = do
    c <- randomConstant
    f <- randomFunction ('c', 0) c
    n <- choose (1::Int, 4::Int)
    if n == 1
    then return $ NF (c, c)
    else if n == 2
         then return $ NF (f, f)
    else if n == 3
         then return $ NF (f, c)
         else return $ NF (c, f)

fUnifiableFOTEf :: Gen NewFOTE
-- Function (...) = Function (...)
fUnifiableFOTEf = do
   fname <- randomName 'f'
   arity <- choose (1::Int, 5::Int)
   (ts1, ts2) <- unifiableListPair arity
   let f1 = Function fname arity ts1
   let f2 = Function fname arity ts2
   return $ NF (f1, f2)




unifiableListPair :: Int -> Gen ([Term],[Term])
unifiableListPair 1 = do
   newfote <- frequency unifiableFOTEGen
   let (t1, t2) = newFOTE2FOTE newfote
   return ([t1],[t2])

unifiableListPair a = do
   newfote <- frequency unifiableFOTEGen
   let (t1, t2) = newFOTE2FOTE newfote
   let t1AsList = t1 : []
   let t2AsList = t2 : []
   (tail1, tail2) <- unifiableListPair (a-1)
   let tailUnifier = unifyTermList tail1 tail2
   let t1AsList' = applySubs (fromJust tailUnifier) t1AsList
   let t2AsList' = applySubs (fromJust tailUnifier) t2AsList
   let subs_head = unifyTerms (head t1AsList') (head t2AsList')
   if subs_head == Nothing
   then unifiableListPair a
   else return ((t1:tail1),(t2:tail2))


unifiableFOTEGen :: [ ( Int, Gen NewFOTE ) ]
unifiableFOTEGen =
  ([(1, vUnifiableFOTEv)]++
   [(4, cUnifiableFOTEf), (5, vUnifiableFOTEf), (1, fUnifiableFOTEf)])



unifiableFOTEGen4Demo :: [ ( Int, Gen NewFOTE ) ]
unifiableFOTEGen4Demo =
  ([(1, vUnifiableFOTEv)]++
   [(1, cUnifiableFOTEf), (1, vUnifiableFOTEf), (50, fUnifiableFOTEf)])


randomFunction :: (Char, Int) -> Term -> Gen Term
-- (Char, Int):
--    special requirement mark
--    'v' means the function has no occurance of a specified variable
--        in the second argument,Int is size parameter
--    'c' means the function shall has the same name as the constant
--        passed as the second argument and has no arguments
--    'a' means arbitrary function, at this time Int is size parameter
randomFunction (c, n) t = do
       if c == 'c'
        then do let name = getName t
                return (Function name 0 [])
        else if c == 'a'
              then do fname  <-  randomName 'f'
                      args   <- sizedListOf sizedTerm (n - 1)
                      let i = length args
                      return (Function fname i args)
              else if c == 'v' then do
                          let s = getName t
                          sizedTermWithout s n
                        else error "randomFunction :: (Char, Int) -> Term -> Gen Term Invalid value for Char"

sizedTermWithout :: String -> Int -> Gen Term
-- without occurance of a variable whose name is specified by String
sizedTermWithout s n = do
  fname  <-  randomName 'f'
  vname  <-  (randomName 'v') `suchThat` (\vn -> vn /= s)
  args   <- sizedListOf goodTerm  (n - 1)
  let i = length args
  elements ([Constant fname, Variable vname] ++
            if n < 2 then []
                     else [Function fname i args])
     where goodTerm = sizedTermWithout s

sizedTerm :: Int -> Gen Term
sizedTerm n = do
  fname  <-  randomName 'f'
  vname  <- randomName 'v'
  args   <- sizedListOf sizedTerm (n - 1)
  let i = length args
  elements ([Constant fname, Variable vname] ++
            if n < 2 then []
                     else [Function fname i args])

-- Given a size s, generates a list where the sizes of the elements sum to <= s
sizedListOf :: (Int -> Gen a) -> Int -> Gen [a]
sizedListOf g s = do
  ns <- listOf (choose (0, s))        -- Generate random points between 0 and s
  let points = sort (take s ns)       -- Sort them
      sizes  = diffsBetween 0 points  -- Get the distances between them
  mapM g (filter (> 0) sizes)         -- Use the distances as element sizes

diffsBetween :: Int -> [Int] -> [Int]
diffsBetween n []     = []
diffsBetween n (x:xs) = (x-n) : diffsBetween x xs
