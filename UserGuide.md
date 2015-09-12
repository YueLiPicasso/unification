#User Guide

##The algorithms unifies first order logic terms. 
##Contents
System requirements (What you need to install on your computer in order to use the software)

Representation of first order logic terms for the algorithms

Loading modules for your calculation

Example sessions

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
[(term_1,term_2),(term_3,term_4)....]

Example: `{a = X, Y = b, Z = X}` is represented as 

`[(Constant "a",Variable "X"), (Variable "Y",constant "b"),(Variable "Z",Variable "X")]` 
