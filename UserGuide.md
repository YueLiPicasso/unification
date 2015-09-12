#User Guide
Information for people who want to use the algorithms to solve their own unification problems. 

##The algorithms works on unifying first order logic terms. 

##You need to install 
GHCi, version >= 7.8.3
##Representation of terms
Constants: `Constant "name_of_constant"`

Example 1: Constant `a` is represented as `Constant "a"`.

Variables: `Variable "name_of_variable"`

Example 2: Variable `X` is represented as `Variable "X"`.

Functions: `Function "function_name" arity [term_1, term_2, ... , term_n]`

Example 3: Function `f (a, X)` is represented as `Function "f" 2 [Constant "a" , Varriable "X"]`

Example 4: Function `g ()`, which has no argument, is represented as `Function "g" 0 []`

Example 5: Function `f (g (y), Y )` is represented as `Function "f" 1 [Function "g" 1 [Constant "y"] , Variable "Y"]`




