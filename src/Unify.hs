{-# LANGUAGE OverloadedStrings #-}
module Unify (unify) where

import Syntax
import Name
import TCMonad
import TypeCheck
import Control.Monad
import Normalize

type Equation = (Term, Term)

-- both sides of every equation are assumed to always be normalized
-- (including eta-long?)
type Equations = [Equation]

normalizeEquations :: Equations -> TC Equations
normalizeEquations = mapM (\(t1, t2) -> (,) <$> normalizeEta t1 <*> normalizeEta t2)

decomposeRigidRigid :: Equations -> TC Equations
decomposeRigidRigid ((t1, t2):rest) = do
  expanded <- rigidRigid t1 t2
  case expanded of
    Just eqs -> decomposeRigidRigid (eqs ++ rest)
    Nothing -> ((t1,t2):) <$> decomposeRigidRigid rest

  where rigidRigid :: Term -> Term -> TC (Maybe Equations)
        rigidRigid (Abs scope1) (Abs scope2) = do
          -- use the same variable to open both scopes
          name <- scopeName scope1
          let var = Var (Free name)
          return . Just $ [(openTerm scope1 var, openTerm scope2 var)]

        rigidRigid tm1 tm2
          | (TermRigid (Rigid hd1 spine1)) <- classifyTerm tm1
          , (TermRigid (Rigid hd2 spine2)) <- classifyTerm tm2 = do
              guard $ hd1 == hd2 && length spine1 == length spine2
              return $ Just (zip spine1 spine2)

        rigidRigid _ _ = return Nothing

decomposeRigidRigid [] = return []

simplify :: Equations -> TC Equations
simplify eqs = normalizeEquations eqs >>= decomposeRigidRigid

data Atom = AtomC Const -- constant
          | AtomV TermName -- free variable
          deriving (Eq, Ord, Show)

data Flex = Flex MetaVar [Term]
data Rigid = Rigid Atom [Term]

data ClassifiedTerm = TermFlex Flex
                    | TermRigid Rigid

data ClassifiedEquation = EqRigidRigid Rigid Rigid
                        | EqFlexRigid Flex Rigid
                        | EqFlexFlex Flex Flex

classifyTerm :: Term -> ClassifiedTerm
classifyTerm t = case hd of
  _ :@ _ -> error "unexpected application in head of term"
  Abs _ -> error "unexpected lambda in head of term"
  Var (Bound _ _) -> error "unexpected bound variable in head of term"
  Var (Free name) -> TermRigid (Rigid (AtomV name) spine)
  Const c -> TermRigid (Rigid (AtomC c) spine)
  Meta m -> TermFlex (Flex m spine)
  where (hd, spine) = collectSpine t

classifyEquation :: Equation -> ClassifiedEquation
classifyEquation (t1, t2) = case (classifyTerm t1, classifyTerm t2) of
  (TermRigid r1, TermRigid r2) -> EqRigidRigid r1 r2
  (TermFlex f, TermRigid r) -> EqFlexRigid f r
  (TermRigid r, TermFlex f) -> EqFlexRigid f r
  (TermFlex f1, TermFlex f2) -> EqFlexFlex f1 f2

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

getSubstitutions :: MetaVar -> [Term] -> Atom -> TC [TC Term]
getSubstitutions mv args1 hd = do
  args1types <- mapM inferType args1
  args1names <- mapM (\ty -> freshFromNameInfo ("x", ty)) args1types

  -- we can only project onto those variables whose result type
  -- matches the expected result type of the metavariable
  resty <- resultType <$> inferType (Meta mv)
  let projVars = map (Var . Free) $ filter (\v -> resultType (snd (nameInfo v)) == resty) args1names

  -- we can only apply imitation when the rigid head is a constant
  -- (and not a free variable)
  let imitateVar = case hd of
        AtomC c -> [Const c]
        _ -> []

  -- this is where a non-deterministic choice happens. hence this is subject to backtracking.
  let possibleAtoms = projVars ++ imitateVar
  return (map (tryAtom args1names) possibleAtoms)

type Substitution = [(MetaVar, Term)]

separateEquations :: Equations -> ([(Flex, Rigid)],[(Flex, Flex)])
separateEquations (e:es) = case classifyEquation e of
  EqRigidRigid _ _ -> error "separateEquations found rigid-rigid equation"
  EqFlexRigid f r -> ((f,r):fr, ff)
  EqFlexFlex f1 f2 -> (fr, (f1,f2):ff)
  where (fr, ff) = separateEquations es
separateEquations [] = ([], [])

data MatchResult = Done [(Flex, Flex)]
                 | Continue (MetaVar, Term) Equations

match :: Equations -> TC MatchResult
match eqs = do
  simplified <- simplify eqs
  let (flexrigid, flexflex) = separateEquations simplified
  case flexrigid of
    [] -> return (Done flexflex)
    ((Flex m args1, Rigid a _):_) -> do
      subs <- getSubstitutions m args1 a

      -- non-deterministic choice, backtracking can occur to try
      -- different options
      s <- msum subs

      return (Continue (m, s) (applySubst m s simplified))

applySubst :: MetaVar -> Term -> Equations -> Equations
applySubst m sub = map (\(t1, t2) -> (substMeta m sub t1, substMeta m sub t2))

unify :: Equations -> TC (Substitution, [(Flex, Flex)])
unify = go []
  where go subs eqs = do
          matchResult <- match eqs
          case matchResult of
            Done flexflex -> return (subs, flexflex)
            Continue sub newEqs -> go (sub:subs) newEqs
