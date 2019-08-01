module Unify where

import Syntax
import TypeCheck
import Control.Monad
import Debug.Trace

type Equation = (Term, Term)

-- both sides of every equation are assumed to always be normalized
-- (including eta-long?)
type Equations = [Equation]

typeCheckEquations :: [Equation] -> TC ()
typeCheckEquations = undefined

failure :: TC a
failure = mzero

decomposeRigidRigid :: Equations -> TC Equations
decomposeRigidRigid ((t1, t2):rest) = do
  expanded <- rigidRigid t1 t2
  case expanded of
    Just eqs -> decomposeRigidRigid (eqs ++ rest)
    Nothing -> do
      srest <- decomposeRigidRigid rest
      return ((t1, t2):srest)
  where rigidRigid :: Term -> Term -> TC (Maybe Equations)
        rigidRigid tm1 tm2
          | (hd1, spine1) <- collectSpine tm1
          , isAtom hd1
          , (hd2, spine2) <- collectSpine tm2
          , isAtom hd2 = do
              guard $ hd1 == hd2 && length spine1 == length spine2
              return $ Just (zip spine1 spine2)

        rigidRigid (Abs scope1) (Abs scope2) = do
          -- use the same variable to open both scopes
          name <- scopeName scope1
          let var = Var (Free name)
          return . Just $ [(openTerm scope1 var, openTerm scope2 var)]

        rigidRigid _ _ = return Nothing

        isAtom (Var (Free _)) = True
        isAtom (Const _) = True
        isAtom (Var (Bound _ _)) = error "unification encountered bound variable"
        isAtom _ = False

decomposeRigidRigid [] = return []
