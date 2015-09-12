#User Guide

##The algorithms unifies first order logic terms. 
##Contents
System requirements (What you need to install on your computer in order to use the software)

Term representation

Doing calculation

License
##You need to install 
GHCi, version >= 7.8.3



##Representation of terms

###Constants

Constant "name_of_constant"

Example: Constant `a` is represented as `Constant "a"`.

###Variables

Variable "name_of_variable"

Example: Variable `X` is represented as `Variable "X"`.

###Functions

Function "function_name" arity [term_1, term_2, ... , term_n]

Example: Function `f (a, X)` is represented as `Function "f" 2 [Constant "a" , Varriable "X"]`

Example: Function `g ()`, which has no argument, is represented as `Function "g" 0 []`

Example: Function `f (g (y), Y )` is represented as `Function "f" 1 [Function "g" 1 [Constant "y"] , Variable "Y"]`

###Equations

(term_1 , term_2)

Example: f (a) = f (X) is represented as `(Function "f" 1 [Constant "a"], Function "f" 1 [Variable "X"])`

###Multiset of Equations 
[(term_1,term_2),(term_3,term_4)...]

Example: `{a = X, Y = b, Z = X}` is represented as 

`[(Constant "a",Variable "X"), (Variable "Y",Constant "b"),(Variable "Z",Variable "X")]` 



##Doing Your Calculation

After you have  GHCi installed, you would need to download the source code by clicking the "Download Zip" button on the home page. You will get a zip file named as `unification-master.zip`. You would need to unzip it to get a folder which contains the source code. The folder will have a default name `unification-master`, if you open this folder, you will find there is a nested folder which is also named as `unification-master`. This (inconvinience, if any) is not in my control. Open  this folder you will find the source code.  Once you have obtained the source code, you can start using the software to unify your terms. 

### Using Robinson's Algorithm

Example: Say you want to unify f (X) with f (a) using Robinson's algorithm, you do the following things:

1. Start your GHCi.

2. Change the search path of GHCi to the folder where the source code is stored. Say you store the codes under directory `C:\Users\Tom\unification-master\unification-master`. To use this directory as search path, type GHCi command 
  `:cd C:\Users\Tom\unification-master\unification-master` 

3. Load module `UnifyTerms` by typing GHCi command `:load UnifyTerms`

4. Call function `unifyTerms` followed by two terms you want to unify. Each term shall be represented according to the aforementioned way and be enclosed in a pair of parenthesis. The two enclosed terms and the  function name shall be separated by spaces. So you would type `unifyTerms (Function "f" 1 [Variable "X"]) (Function "f" 1 [Constant "a"])` then hit `Enter`.

5. To continue calculation for another pair of terms, simply repeat step 4 (of course you would change the input for `unifyTerms`) 

####Display of result

The substitution is what you will get when using Robinson's algorithm to solve your first order logic unification problems.
Substituting `a` for `X` is displayed as `(a, X)`. `[(a,X),(b,Y),(Z,K)]` stands for the substitution `{a/X,b/Y,Z/K}`.

When the terms can't be unified, you will see the word "Nothing" being displayed otherwise you would see "Just" being displayed follwed by the substitution. So "Nothing" and "Just [(a,X),(b,Y)]" are the typical results you might expect from the program. For the given example, the result is "Just [(a,X)]".

###Using Martelli's Algorithm

Example: Say you want to solve equation set { f (X) = f (a) , b = Y } using Martelli's algorithm, you do the following things:

1. Start your GHCi.

2. Change the search path of GHCi to the folder where the source code is stored. Say you store the codes under directory `C:\Users\Tom\unification-master\unification-master`. To use this directory as search path, type GHCi command 
  `:cd C:\Users\Tom\unification-master\unification-master` 

3. Load module `MMAlgoA` by typing GHCi command `:load MMAlgoA`

4. Call function `unificationTransform` followed by the equation set you want to unify. The equation set shall be represented according to the aforementioned way. So you would type `unificationTransform [ ( Function "f" 1 [Variable "X"], Function "f" 1 [Constant "a"] ), (Constant "b", Variable "Y") ]` then hit `Enter`.

5. To continue calculation for another equation set, simply repeat step 4 (of course you would change the input for `unificationTransform`) 

####Display of result

If the equation set can't be solved, the word "Nothing" would be displayed. If it can be solved, the solved form of the original equation set would be displayed following a word "Just", so for the given example,  you would see "Just [(X,a),(Y,b)]", representing the solved form { X=a ,Y=b } of the original equation set  { f (X) = f (a) , b = Y }. 
##License

This software uses GNU GPL license, so you can download it, study it, modify it and give others copy of original code or your modified version. When other people obtain a copy of the original or modified software from you, they shall also have the right to study, modify and distribute that software. For details, search "GNU General Public License" on Wikipedia, if you don't want to read the license itself.


