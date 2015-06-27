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
import ReadPrintTerms(Term(..), getName, isFunction)
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
-- Variable symbol = Variable Symbol
vUnifiableFOTEv = do
      v1 <- randomVariable
      n  <- choose (1::Int, 2::Int)  -- half of time gives x = x the other half x = y
      if n == 1 then return $ NF (v1, v1)
                else do v2 <- randomVariable
                        return $ NF (v1, v2)

cUnifiableFOTEc :: Gen NewFOTE
-- Constant symbol = Constant Symbol
cUnifiableFOTEc = do
      c <- randomConstant
      return $ NF (c, c)

vUnifiableFOTEc :: Gen NewFOTE
-- Variable symbol = Constant Symbol or
-- commutation of above
vUnifiableFOTEc = do
      v <- randomVariable
      c <- randomConstant
      n  <- choose (1::Int, 2::Int)
      -- half of time gives c = v the other half v = c
      if n == 1 then return $ NF (v, c)
                else return $ NF (c, v)

vUnifiableFOTEf :: Gen NewFOTE
--Variable Symbol = Function or
--commutation of above
vUnifiableFOTEf = do
      v <- randomVariable
      s <- choose (2, 3)
      f <- randomFunction ('v', s ) v
      n <- choose (1::Int, 2::Int)
      if n == 1 then return $ NF (v, f)
                else return $ NF (f, v)

cUnifiableFOTEf :: Gen NewFOTE
cUnifiableFOTEf = do
    c <- randomConstant
    f <- randomFunction ('c', 0) c
    n <- choose (1::Int, 3::Int)
    if n == 1
    then return $ NF (c, c)
    else if n == 2
         then return $ NF (f, c)
         else return $ NF (c, f)

fUnifiableFOTEf :: Gen NewFOTE
fUnifiableFOTEf = do
   fname <- randomName 'f'
   arity <- choose (0::Int, 5::Int)
   if arity /= 0 then do
   (ts1, ts2) <- unifiableListPair arity
   let f1 = Function fname arity ts1
   let f2 = Function fname arity ts2
   return $ NF (f1, f2)
   else do let f1 = Function fname 0 []
           let f2 = Function fname 0 []
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
  ([(5, vUnifiableFOTEv), (5,cUnifiableFOTEc), (40,vUnifiableFOTEc)]++
   [(1, cUnifiableFOTEf), (5, vUnifiableFOTEf), (5, fUnifiableFOTEf)])



unifiableFOTEGen4Demo :: [ ( Int, Gen NewFOTE ) ]
unifiableFOTEGen4Demo =
  ([(1, vUnifiableFOTEv), (1,cUnifiableFOTEc), (1,vUnifiableFOTEc)]++
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
                          (sizedTermWithout s n) `suchThat` (\t-> isFunction t )
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
