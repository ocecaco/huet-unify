module Unify where

import Syntax
import TypeCheck

type Equation = (Term, Term)

-- both sides of every equation are assumed to always be normalized
-- (including eta-long?)
type Equations = [Equation]

typeCheckEquations :: [Equation] -> TC ()
typeCheckEquations = undefined

failure :: TC a
failure = undefined

simplify :: Equations -> TC Equations
simplify = undefined
