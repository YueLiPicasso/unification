#User Guide
Information for people who want to use the algorithms to solve their own unification problems. The algorithms works on unifying first order logic terms. 

##You need to install 
GHCi, version >= 7.8.3
##Representation of terms
Constants: `Constant "name_of_constant"`
Example: Constant `a` is represented as `Constant "a"`.
Variables: `Variable "name_of_variable"`
Example: Variable `X` is represented as `Variable "X"`.
Functions: `Function "function_name" arity [term_1, term_2...term_n]`
Example: 
