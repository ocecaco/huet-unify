{-# LANGUAGE OverloadedStrings #-}
module Unify (unify) where

import Syntax
import Name
import TCMonad
import TypeCheck
import Control.Monad
import Normalize
import Data.List (partition)

type Equation = (Term, Term)

-- both sides of every equation are assumed to always be normalized
-- (including eta-long?)
type Equations = [Equation]

normalizeEquations :: Equations -> TC Equations
normalizeEquations eqs = mapM (\(t1, t2) -> (,) <$> normalizeEta t1 <*> normalizeEta t2) eqs

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
              if hd1 == hd2 && length spine1 == length spine2
                then return ()
                else typeError "rigid-rigid mismatch"
              return $ Just (zip spine1 spine2)

        rigidRigid _ _ = return Nothing

decomposeRigidRigid [] = return []

isAtom :: Term -> Bool
isAtom (Var (Free _)) = True
isAtom (Const _) = True
isAtom (Var (Bound _ _)) = error "unification encountered bound variable"
isAtom _ = False

matchFlexRigid :: Equation -> TC (MetaVar, [TC Term])
matchFlexRigid (t1, t2)
  | (Meta m, args1) <- collectSpine t1
  , (hd, _) <- collectSpine t2
  , isAtom hd = fmap (\subs -> (m, subs)) $ getSubstitutions m args1 hd

matchFlexRigid (t2, t1)
  | (Meta m, args1) <- collectSpine t1
  , (hd, _) <- collectSpine t2
  , isAtom hd = fmap (\subs -> (m, subs)) $ getSubstitutions m args1 hd

matchFlexRigid _ = error "matchFlexRigid applied to flex-flex equations"

isConst :: Term -> Bool
isConst (Const _) = True
isConst _ = False

tryAtom :: [TermName] -> Term -> TC Term
tryAtom argNames chosenAtom = do
  let argTys = map (snd . nameInfo) argNames
  let argVars = map (Var . Free) argNames
  atomType <- inferType chosenAtom
  let atomArgTypes = argTypes atomType

  -- create fresh meta variables of the proper type for the arguments
  -- of the chosen atom
  metaNames <- mapM freshFromNameInfo [ createArrowType argTys atomArgty | atomArgty <- atomArgTypes ]
  let metaHeads = map (Meta . MetaVar) metaNames
  let appliedMetas = map (\metahd -> createSpine metahd argVars) metaHeads

  let appliedAtom = createLambdas argNames $ createSpine chosenAtom appliedMetas

  return appliedAtom

getSubstitutions :: MetaVar -> [Term] -> Term -> TC [TC Term]
getSubstitutions mv args1 hd = do
  args1types <- mapM inferType args1
  args1names <- mapM (\ty -> freshFromNameInfo ("x", ty)) args1types

  -- we can only project onto those variables whose result type
  -- matches the expected result type of the metavariable
  resty <- resultType <$> inferType (Meta mv)
  let projVars = map (Var . Free) $ filter (\v -> resultType (snd (nameInfo v)) == resty) args1names

  -- we can only apply imitation when the rigid head is a constant
  -- (and not a free variable)
  let imitateVar = if isConst hd then [hd] else []

  -- this is where a non-deterministic choice happens. hence this is subject to backtracking.
  let possibleAtoms = projVars ++ imitateVar
  return (map (tryAtom args1names) possibleAtoms)

type Substitution = [(MetaVar, Term)]

isFlexible :: Term -> Bool
isFlexible (Meta _) = True
isFlexible (t :@ _) = isFlexible t
isFlexible _ = False

match :: Equations -> TC (Maybe (MetaVar, Term), Equations)
match eqs = do
  eqsNormalized <- normalizeEquations eqs
  decomposed <- decomposeRigidRigid eqsNormalized
  let (flexflex, flexrigid) = partition (\(t1, t2) -> isFlexible t1 && isFlexible t2) decomposed
  case flexrigid of
    [] -> return (Nothing, flexflex)
    (f:_) -> do
      (m, subs) <- matchFlexRigid f
      -- non-deterministic choice, backtracking can occur to try
      -- different options
      s <- tryFinite subs
      return (Just (m, s), applySubst m s decomposed)

applySubst :: MetaVar -> Term -> Equations -> Equations
applySubst m sub = map (\(t1, t2) -> (substMeta m sub t1, substMeta m sub t2))

unify :: Equations -> TC (Substitution, Equations)
unify = go []
  where go subs eqs = do
          (maybeSub, newEqs) <- match eqs
          case maybeSub of
            Nothing -> return (subs, newEqs)
            Just sub -> go (sub:subs) newEqs
