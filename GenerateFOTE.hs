module GenerateFOTE where
-- generate unifiable FOTEs
import Data.List
import Test.QuickCheck
import MMAlgoA
import ReadPrintTerms(Term(..), getName, isFunction)
import FOTEset (NewFOTE(..))

instance Arbitrary Term where
  arbitrary = do
    Positive size <- arbitrary
    sizedTerm (size `mod` 1000)

randomName :: Char -> Gen String
-- generate random two letter length function or Variable name
-- 'f' for generating function name, other char for variable name
randomName c = do
   nameTail <- elements (['0'..'9']++['a'..'z'])
   if c == 'f' then do fnameHead <- elements ['a'..'z']
                       return (fnameHead : nameTail : [])
               else do vnameHead <- elements ['A'..'Z']
                       return (vnameHead : nameTail: [])


randomConstant :: Gen Term
randomConstant = do
       fname  <-  randomName 'f'
       return (Constant fname)

randomVariable :: Gen Term
randomVariable = do
        vname  <- randomName 'v'
        return (Variable vname)

vUnifiableFOTEv :: Gen NewFOTE -- Variable symbol = Variable Symbol
vUnifiableFOTEv = do
      v1 <- randomVariable
      n  <- choose (1::Int, 2::Int)  -- half of time gives x = x the other half x = y
      if n == 1 then return $ NF (v1, v1)
                else do v2 <- randomVariable
                        return $ NF (v1, v2)
vUnifiableFOTEf :: Gen NewFOTE --Variable Symbol = Function
vUnifiableFOTEf = undefined
--      v <- randomVariable
--      f <- randomFunction ('v', 0) v




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
