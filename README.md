# Automatic differentiation for R

This is a very initial implementation of an algebraic AST to represent
R expressions that generates gradient functions through backward-mode automatic
differentiation. It currently does some simple rewriting to simplify
expressions and generates functions with some common subexpression elimination.
