{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Unify (unify, runUnify) where

-- TODO: Maybe type-check the equations once before starting the
-- unification process? Otherwise just let the user do it himself.

-- TODO: Add a parser/pretty printer so we can easily write equations.

import Syntax
import Name
import TCMonad
import TypeCheck
import Control.Monad
import Normalize
import Control.Monad.Logic
import Control.Applicative (Alternative)
import qualified Data.Set as S
import Data.Set (Set)

type Equation = (Term, Term)

newtype Unify a = Unify { _unUnify :: LogicT TC a }
                deriving (Functor, Applicative, Monad, Alternative, MonadPlus, MonadLogic)

-- both sides of every equation are assumed to always be normalized
-- (including eta-long?)
type Equations = [Equation]

liftTC :: TC a -> Unify a
liftTC = Unify . lift

normalizeEquations :: Equations -> TC Equations
normalizeEquations = mapM (\(t1, t2) -> (,) <$> normalizeEta t1 <*> normalizeEta t2)

decomposeRigidRigid :: Equations -> Unify Equations
decomposeRigidRigid ((t1, t2):rest) = do
  expanded <- rigidRigid t1 t2
  case expanded of
    Just eqs -> decomposeRigidRigid (eqs ++ rest)
    Nothing -> ((t1,t2):) <$> decomposeRigidRigid rest

  where rigidRigid :: Term -> Term -> Unify (Maybe Equations)
        rigidRigid (Abs scope1) (Abs scope2) = do
          -- use the same variable to open both scopes
          name <- liftTC $ scopeName scope1
          let var = Var (Free name)
          return . Just $ [(openTerm scope1 var, openTerm scope2 var)]

        rigidRigid tm1 tm2
          | (TermRigid (Rigid hd1 spine1)) <- classifyTerm tm1
          , (TermRigid (Rigid hd2 spine2)) <- classifyTerm tm2 = do
              guard $ hd1 == hd2
              -- don't need to check that the lengths of the spines
              -- match, since this is ensured by the fact that the
              -- equation is type-correct and both terms are
              -- eta-expanded
              -- guard $ length spine1 == length spine2
              return $ Just (zip spine1 spine2)

        rigidRigid _ _ = return Nothing

decomposeRigidRigid [] = return []

data Atom = AtomC Const -- constant
          | AtomV TermName -- free variable
          deriving (Eq, Ord, Show)

data Flex = Flex MetaVar [Term]
          deriving (Eq, Ord, Show)

data Rigid = Rigid Atom [Term]
           deriving (Eq, Ord, Show)

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

trySubstitution :: MetaVar -> [Ty] -> Atom -> Unify Term
trySubstitution mv argsTys hd = do
  argNames <- mapM (\ty -> liftTC $ fresh ("x", ty)) argsTys
  let argVars = map (Var . Free) argNames
  -- we can only apply imitation when the rigid head is a constant
  -- (and not a free variable)
  let imitateVar = case hd of
        AtomC c -> [Const c]
        _ -> []
  let possibleAtoms = argVars ++ imitateVar

  -- CAREFUL: non-deterministic choice happens here
  chosenAtom <- msum (map return possibleAtoms)

  -- result type of the chosen atom should match the expected result
  -- type of the metavariable
  atomType <- liftTC $ inferType chosenAtom
  resty <- liftTC $ resultType <$> inferType (Meta mv)
  guard (resultType atomType == resty)

  -- create fresh meta variables of the proper type for the arguments
  -- of the chosen atom
  let atomArgTypes = argTypes atomType
  metaNames <- mapM (liftTC . fresh) [ createArrowType argsTys atomArgty | atomArgty <- atomArgTypes ]
  let metaHeads = map (Meta . MetaVar) metaNames
  let appliedMetas = map (\metahd -> createSpine metahd argVars) metaHeads

  let appliedAtom = createLambdas argNames $ createSpine chosenAtom appliedMetas

  return appliedAtom

type Substitution = (MetaVar, Term)
type Substitutions = [Substitution]

separateEquations :: Equations -> ([(Flex, Rigid)],[(Flex, Flex)])
separateEquations (e:es) = case classifyEquation e of
  EqRigidRigid _ _ -> error "separateEquations found rigid-rigid equation"
  EqFlexRigid f r -> ((f,r):fr, ff)
  EqFlexFlex f1 f2 -> (fr, (f1,f2):ff)
  where (fr, ff) = separateEquations es
separateEquations [] = ([], [])

data MatchResult = Done [(Flex, Flex)]
                 | Continue Substitution Equations

match :: Equations -> Unify MatchResult
match eqs = do
  decomposed <- decomposeRigidRigid eqs
  let (flexrigid, flexflex) = separateEquations decomposed
  case flexrigid of
    [] -> return (Done flexflex)
    ((Flex m args1, Rigid a _):_) -> do
      args1types <- mapM (liftTC . inferType) args1
      s <- trySubstitution m args1types a
      Continue (m, s) <$> liftTC (applySubstEquations m s decomposed)

applySubstEquations :: MetaVar -> Term -> Equations -> TC Equations
applySubstEquations m sub eqs = normalizeEquations (map (\(t1, t2) -> (substMeta m sub t1, substMeta m sub t2)) eqs)

applySubstSubstitutions :: MetaVar -> Term -> Substitutions -> TC Substitutions
applySubstSubstitutions m sub = mapM (\(m2, t) -> (m2,) <$> normalize (substMeta m sub t))

metaVarsEquations :: Equations -> Set MetaVar
metaVarsEquations eqs = S.unions (map (\(t1, t2) -> metaVars t1 `S.union` metaVars t2) eqs)

unify :: Equations -> Unify (Substitutions, [(Flex, Flex)])
unify originalEqs = do
  -- normalize the equations once at the beginning. afterwards, the
  -- equations are kept normalized by normalizing them every time we
  -- perform a substitution for some meta-variables
  normalized <- liftTC $ normalizeEquations originalEqs
  let relevantMetaVars = metaVarsEquations normalized
  go relevantMetaVars [] normalized
  where go relevant oldSubs eqs = do
          matchResult <- match eqs
          case matchResult of
            Done flexflex -> return (oldSubs, flexflex)
            Continue (m,s) newEqs
              -- only keep track of this substitution if it pertains
              -- to a metavariable that was originally in the
              -- equations (and not generated along the way)
              | m `S.member` relevant -> do
                  newSubs <- ((m,s):) <$> liftTC (applySubstSubstitutions m s oldSubs)
                  go relevant newSubs newEqs
              | otherwise -> do
                  newSubs <- liftTC (applySubstSubstitutions m s oldSubs)
                  go relevant newSubs newEqs

runUnify :: Maybe Int -> Unify a -> Either TCError [a]
runUnify maybeBound (Unify act) = runTC (runner act)
  where runner x = case maybeBound of
          Just b -> observeManyT b x
          Nothing -> observeAllT x
