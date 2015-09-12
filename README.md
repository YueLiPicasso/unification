# Implementing Unification Algorithms in Haskell      

Welcome! 

This document is intended to help MSc project examiners browse the code and test run the code on their own computers. If you are a user, or you are an examiner but you  want to see the user guide, please look into the file [UserGuide.md (click me)](UserGuide.md).  

##Getting the Source Code to you local disc
The easiest way is to click "Download Zip" button on the [Repository Home](https://github.com/YueLiPicasso/unification). The downloaded zip file will be named as `unification-master.zip`
###Prepare your computer to run the code
1. After you download the zip file, you would need to find it, unzip it and store the unzipped folder in a directory, say `C:\Users\Yue\Downloads`.
2. Start your GHCi and change the search path to the `unification-master` folder by typing GHCi command

 `:cd C:\Users\Yue\Downloads\unification-master\unification-master`
 

##If you wanted to see the code for Robinson's Algorithm

###For the data type
See module [ReadPrintTerms](ReadPrintTerms.hs)
###For occurs check
See module [ReadPrintTerms](ReadPrintTerms.hs)
###For substitution
See module [Substitution](Substitution.hs)
###For the main algorithm body
See module [UnifyTerms](UnifyTerms.hs). To run this algorithm please load the module into your Haskell platform such as WinGHCi, which is used by the author. Then you can call the function `unifyTerms`.

Example function calls  can be find in [testUnifyTerms.txt](testUnifyTerms.txt)

##If you wanted to see the code for Martelli's Algorithm

###For the data type
See both modules [ReadPrintTerms](ReadPrintTerms.hs) and [FOTEset](FOTEset.hs)
###For occurs check
See module [ReadPrintTerms](ReadPrintTerms.hs)
###For the main algorithm body
See module [MMAlgoA](MMAlgoA.hs). To run this algorithm please load the module into your Haskell platform such as WinGHCi, which is used by the author. Then you can call the function `unificationTransform`.

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
See module [GenerateFOTE](GenerateFOTE.hs). Robinson's agorithm was used in both generators.

To sample the generators: 

1. Load the module `GenerateFOTE` into GHCi.
2. Type GHCi command `sample $ frequency unifiableFOTEGen`, then hit `Enter`. Some randomly generated unifiable first order term equations will be displayed.You can run this command for as many times as you like.
3. Type GHCi command `sample $ frequency nUFOTEGen`, then hit `Enter`. Some randomly generated not unifiable first order term equations will be displayed.You can run this command for as many times as you like.

###For property inspection
See file [testFunctions.hs](testFunctions.hs). The comprehensive test was done by functions in the first 4 blocks of function definition. 

To test that the Martelli's algorithm always returns Fail for not solvable equations:

1. Load the `testFunctions.hs` file into GHCi by typing GHCi command `:load testFunctions`.
2. Type GHCi command `quickCheck prop_notUnify_notUnifiableFOTEMM` then hit `Enter`. You may run this command for as many times as you want. 

To test that the Martelli's algorithm always returns correct unifiers for solvable equations:

1. Load the `testFunctions.hs` file into GHCi by typing GHCi command `:load testFunctions`. If the file has already been loaded, then skip this step.
2. Type GHCi command `quickCheck prop_unifCorrectMM` then hit `Enter`. You may run this command for as many times as you want.

##If you wanted to see the code for project demonstration

See module [UnificationDemo](UnificationDemo.hs). The demonstration can use Robinson's algorithm to generate random solvable or not solvable euqations and pass these equations to Martelli's algorithm to process, then display the  result.

To run the demonstration:

1. Load module `UnificationDemo` into GHCi by typing GHCi command `:load UnificationDemo`.
2. Type GHCi command `unificationDemo "MM1976A"` then hit `Enter`. You may run this command for as many times as you like. 

##If you wanted to know what other files are about, which were not mentioned above
`terms2read.txt` : List of terms to read, made for a early program.

`Setup.hs` and `unification.cabal` : Two files that makes this repository a cabal package for distribution.

`MMAlgoA.Buggy.hs`: A copy of module MMAlgoA when it had problems. 

`LICENSE` : Terms and conditions for using the code. 

`Test.hs` : Term generator and property checks written by Mr. Chris Warburton.

`.gitignore` : A file that specifies what files shall be ignored when back up the local repositiry to GitHub.
