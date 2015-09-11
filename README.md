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
see module [UnifyTerms](UnifyTerms.hs)

##If you wanted to see the code for Martelli's Algorithm

###For the data type
see both modules [ReadPrintTerms](ReadPrintTerms.hs) and [FOTEset](FOTEset.hs)
###For occurs check
see module [ReadPrintTerms](ReadPrintTerms.hs)
###For the main algorithm body
see module [MMAlgoA](MMAlgoA.hs)


##If you wanted to see the code for test
\label{sec: code for test}
\begin{description}
\item[For the equation generators] see module \emph{GenerateFOTE}
\item[For property inspection] see file \emph{testFunctions.hs}
\end{description}

\section[Code for Demonstration]{If you wanted to see the code for project demonstration}
\label{sec: code for demo}
See module \emph{UnificationDemo}

\section[Other files]{If you wanted to know what other files are about, which were not mentioned in Section~\protect\ref{sec: code for rob},~\protect\ref{sec: code for mar},~\protect\ref{sec: code for test} and~\protect\ref{sec: code for demo}}
\begin{tabular}{r@{.}l|p{7cm}}
terms2read & txt & List of terms to read, made for a early program.~~~~~~~~~~~~~~~~~~~~~~~~~\\
testUnifyTerms & txt & Recording of test of Robinson's algorithm using hand made data.\\
MMAlgoA.Buggy & hs & A copy of module \emph{MMAlgoA} when it had problems. \\
testMMAlgoA& txt & A hand made equation set used to test Martelli's algorithm.\\
testMMAlgoA\_2 & txt  & A hand made equation set used to test Martelli's algorithm.\\
Test& hs & Term generator and property checks written by Mr. Chris Warburton.\\
\end{tabular}
