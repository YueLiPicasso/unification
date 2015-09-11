# Implementing Unification Algorithms in Haskell      

Welcome!

##Getting the Source Code to you local disc
You can click "Download Zip" button on the [Repository Home](https://github.com/YueLiPicasso/unification)

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
see module [MMAlgoA](MMAlgoA.hs)


##If you wanted to see the code for test

###For the equation generators
see module [GenerateFOTE](GenerateFOTE.hs)
###For property inspection
see file [testFunctions.hs](testFunctions.hs)


##If you wanted to see the code for project demonstration

See module [UnificationDemo](UnificationDemo.hs)

##If you wanted to know what other files are about, which were not mentioned above
`terms2read.txt` : List of terms to read, made for a early program.


`MMAlgoA.Buggy.hs`: A copy of module \emph{MMAlgoA} when it had problems. 

`testMMAlgoA.txt` : A hand made equation set used to test Martelli's algorithm.

`testMMAlgoA\_2.txt`: A hand made equation set used to test Martelli's algorithm.

`Test.hs` : Term generator and property checks written by Mr. Chris Warburton.

