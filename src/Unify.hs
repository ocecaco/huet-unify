{-# LANGUAGE OverloadedStrings #-}
module Unify () where

import Syntax
import Name
import TCMonad
import TypeCheck
import Control.Monad

type Equation = (Term, Term)

-- both sides of every equation are assumed to always be normalized
-- (including eta-long?)
type Equations = [Equation]

decomposeRigidRigid :: Equations -> TC Equations
decomposeRigidRigid ((t1, t2):rest) = do
  expanded <- rigidRigid t1 t2
  case expanded of
    Just eqs -> decomposeRigidRigid (eqs ++ rest)
    Nothing -> do
      srest <- decomposeRigidRigid rest
      return ((t1, t2):srest)
  where rigidRigid :: Term -> Term -> TC (Maybe Equations)
        rigidRigid (Abs scope1) (Abs scope2) = do
          -- use the same variable to open both scopes
          name <- scopeName scope1
          let var = Var (Free name)
          return . Just $ [(openTerm scope1 var, openTerm scope2 var)]

        rigidRigid tm1 tm2
          | (hd1, spine1) <- collectSpine tm1
          , isAtom hd1
          , (hd2, spine2) <- collectSpine tm2
          , isAtom hd2 = do
              guard $ hd1 == hd2 && length spine1 == length spine2
              return $ Just (zip spine1 spine2)

        rigidRigid _ _ = return Nothing

decomposeRigidRigid [] = return []

isAtom :: Term -> Bool
isAtom (Var (Free _)) = True
isAtom (Const _) = True
isAtom (Var (Bound _ _)) = error "unification encountered bound variable"
isAtom _ = False

matchFlexRigid :: Equation -> Maybe (TC (MetaVar, Term))
matchFlexRigid (t1, t2)
  | (Meta m, args1) <- collectSpine t1
  , (hd, args2) <- collectSpine t2
  , isAtom hd = Just $ fmap (\subs -> (m, subs)) $ trySubstitutions m args1 hd args2

matchFlexRigid (t2, t1)
  | (Meta m, args1) <- collectSpine t1
  , (hd, args2) <- collectSpine t2
  , isAtom hd = Just $ fmap (\subs -> (m, subs)) $ trySubstitutions m args1 hd args2

-- it's a flex-flex pair, hence we should skip it and not try any substitutions
matchFlexRigid _ = Nothing

isConst :: Term -> Bool
isConst (Const _) = True
isConst _ = False

trySubstitutions :: MetaVar -> [Term] -> Term -> TC Term
trySubstitutions mv args1 hd = do
  resty <- resultType <$> inferType (Meta mv)

  args1types <- mapM inferType args1
  args1names <- mapM (\ty -> freshFromNameInfo ("x", ty)) args1types
  let args1vars = map (Var . Free) args1names

  -- we can only project onto those variables whose result type
  -- matches the expected result type of the metavariable
  let projVars = map (return . Var . Free) $ filter (\v -> resultType (snd (nameInfo v)) == resty) args1names

  -- we can only apply imitation when the rigid head is a constant
  -- (and not a free variable)
  let imitateVar = if isConst hd then [return hd] else []

  -- this is where a non-deterministic choice happens. hence this is subject to backtracking.
  chosenAtom <- msum (projVars ++ imitateVar)

  atomType <- inferType chosenAtom
  let atomArgTypes = argTypes atomType

  -- create fresh meta variables of the proper type for the arguments
  -- of the chosen atom
  metaNames <- mapM freshFromNameInfo [ createArrowType args1types argty | argty <- atomArgTypes ]
  let metaHeads = map (Meta . MetaVar) metaNames
  let appliedMetas = map (\metahd -> createSpine metahd args1vars) metaHeads

  let appliedAtom = createLambdas args1names $ createSpine chosenAtom appliedMetas

  return appliedAtom
