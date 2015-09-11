# Implementing Unification Algorithms in Haskell      

Welcome!

##Getting the Source Code to you local disc
The easiest way is to click "Download Zip" button on the [Repository Home](https://github.com/YueLiPicasso/unification)

##If you wanted to see the code for Robinson's Algorithm

###For the data type
see module [ReadPrintTerms](ReadPrintTerms.hs)
###For occurs check
see module [ReadPrintTerms](ReadPrintTerms.hs)
###For substitution
see module [Substitution](Substitution.hs)
###For the main algorithm body
see module [UnifyTerms](UnifyTerms.hs). To run this algorithm please load the module into your Haskell platform such as WinGHCi, which is used by the author. Then you can call the function `unifyTerms`.

Example function calls  can be find in [testUnifyTerms.txt](testUnifyTerms.txt)

##If you wanted to see the code for Martelli's Algorithm

###For the data type
see both modules [ReadPrintTerms](ReadPrintTerms.hs) and [FOTEset](FOTEset.hs)
###For occurs check
see module [ReadPrintTerms](ReadPrintTerms.hs)
###For the main algorithm body
see module [MMAlgoA](MMAlgoA.hs). To run this algorithm please load the module into your Haskell platform such as WinGHCi, which is used by the author. Then you can call the function `unificationTransform`.

An example function call can be found in [testMMAlgoA.txt](testMMAlgoA.txt). 

Another example input is in [testMMAlgoA_2.txt](testMMAlgoA_2.txt). To use this file:

1. Load the module `MMalgoA` to GHCi.
2. Type GHCi command `let eSet=readFile "testMMAlgoA_2.txt"`, then hit `Enter`. Nothing would happen.
3. Type GHCi command `eSet`, then hit `Enter`. You will see the content of the file being displayed.
4. Type GHCi command `let eSet'= (read it)::FOTEset`, then hit `Enter`. Nothing would happen.
5. Type GHCi command `eSet'`, then hit `Enter`. The equation will be displayed.
6. Type GHCi command `unificationTransform eSet'` to solved the equation. The result would be displayed.


##If you wanted to see the code for test

###For the equation generators
see module [GenerateFOTE](GenerateFOTE.hs).

To sample the generators: 

1. Load the module `GenerateFOTE` into GHCi.
2. Type GHCi command `sample $ frequency unifiableFOTEGen`, then hit `Enter`. Some randomly generated unifiable first order term equations will be displayed.You can run this command as many times as you like.
3. Alternative to 2, Type GHCi command `sample $ frequency nUFOTEGen`, then hit `Enter`. Some randomly generated not unifiable first order term equations will be displayed.You can run this command as many times as you like.

###For property inspection
see file [testFunctions.hs](testFunctions.hs)


##If you wanted to see the code for project demonstration

See module [UnificationDemo](UnificationDemo.hs)

##If you wanted to know what other files are about, which were not mentioned above
`terms2read.txt` : List of terms to read, made for a early program.


`MMAlgoA.Buggy.hs`: A copy of module \emph{MMAlgoA} when it had problems. 


`Test.hs` : Term generator and property checks written by Mr. Chris Warburton.

