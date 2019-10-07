# huet-unify
This is a Haskell implementation of Huet's pre-unification algorithm for the simply typed lambda calculus. Huet's algorithm is a well-known algorithm for solving higher-order unification problems. The paper originally describing it is "A Unification Algorithm for Typed Lambda-Calculus" by Gérard Huet. Higher-order unification is the task of finding functions in the simply-typed lambda calculus which satisfy a given set of equations. For example, if you give Huet's algorithm the following problem:
```
types
  int
constants
  zero : int
  one : int
metavariables
  F : int -> int
equations
  F zero = zero
  F one = one
```
Then it will output that `F` should be `fun x => x`, the identity function, in order to satisfy the given equations. Unfortunately, the task of solving higher-order unification problems is in general semi-decidable. This means that Huet's algorithm will always find all functions which work to solve the set of equations, but it might end up looping forever because it does not "know" whether there might be more solutions. In general, multiple solutions might exist, and all of them will be found by backtracking search.

In constrast to some other implementations I found online, my implementation actually uses the type information to guide the search, which is necessary in a faithful implementation of Huet's algorithm. The use of type information is also what makes it tricky to scale Huet's algorithm directly to dependently-typed languages, although variations which work for dependently-typed languages do exist. 

Furthermore, my implementation assumes that equality of lambda terms includes eta equality, meaning that we consider the functions `fun x => f x` and `f` to be equal. This considerably simplifies the implementation (as mentioned in Huet's paper).
