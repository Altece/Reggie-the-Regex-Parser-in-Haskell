# Reggie the Regex Parser in Haskell

This project is an attempt to rethink the ideas behind the original Reggie the Regex Parser 
project in a higher-level hanguage.

The original project was an attempt to create a regular expression parser in pure C by
building up a non-deterministic push-down automata simulation that could be used to 
evaluate a given regular expression and build up a non-deterministic finite automata structure
that could then be evaluated against strings.
 
Keeping everything in C resulted in more time being spent building up higher-level constructs than
solving the problem of how to modify the push-down automata to build up a data structure. This project
is an attempt to think about the automata at a higher level.