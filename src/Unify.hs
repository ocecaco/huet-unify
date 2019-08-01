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
simplify ((t1, t2):rest) = do
  expanded <- rigidRigid t1 t2
  case expanded of
    Just eqs -> simplify (eqs ++ rest)
    Nothing -> do
      srest <- simplify rest
      return ((t1, t2):srest)
  where rigidRigid :: Term -> Term -> TC (Maybe Equations)
        rigidRigid (left1 :@ left2) (right1 :@ right2) = return . Just $ [(left1, right1), (left2, right2)]
        rigidRigid (Abs scope1) (Abs scope2) = do
          -- use the same variable to open both scopes
          name <- scopeName scope1
          let var = Var (Free name)
          return . Just $ [(openTerm scope1 var, openTerm scope2 var)]
        rigidRigid (Var (Bound _ _)) _ = error "unification encountered bound variables"
        rigidRigid _ (Var (Bound _ _)) = error "unification encountered bound variables"
        rigidRigid (Var (Free name1)) (Var (Free name2))
          | name1 == name2 = return . Just $ []
          | otherwise = failure
        rigidRigid (Const c1) (Const c2)
          | c1 == c2 = return . Just $ []
          | otherwise = failure
        rigidRigid _ _ = return Nothing

simplify [] = return []
