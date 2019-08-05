module Syntax
  ( Term(..)
  , Ty(..)
  , Const(..)
  , MetaVar(..)
  , TermName
  , TermVar
  , TermScope
  , bindTerm
  , openTerm
  , unbindTerm
  , substTerm
  , substMeta
  , substVar
  , metaVars
  , scopeName
  , argTypes
  , resultType
  , createArrowType
  , collectLambdas
  , createLambdas
  , collectSpine
  , createSpine
  )
where

import Data.Text (Text)
import Data.List (foldl')
import qualified Data.Set as S
import Data.Set (Set)
import Name

type TermName = Name (Text, Ty)
type TermVar = Var (Text, Ty) ()
type TermScope = Scope (Ignore Text, Ty) Term

data MetaVar = MetaVar (Name Ty)
             deriving (Eq, Ord, Show)

data Const = C Text Ty
            deriving (Eq, Ord, Show)

data Term = Term :@ Term -- application
          | Abs TermScope -- lambda-abstraction
          | Var TermVar -- free/bound variable
          | Const Const -- constants
          | Meta MetaVar -- metavariables
          deriving (Eq, Ord, Show)

data Ty = Ty :-> Ty
        | BaseTy Text
        deriving (Eq, Ord, Show)

infixr :->
infixl :@

bindTerm :: TermName -> Term -> TermScope
bindTerm bindname bindterm = ManualScope (Ignore prettyname, bindty) (go 0 bindterm)
  where go :: Int -> Term -> Term
        go _ tm@(Const _) = tm
        go _ tm@(Meta _) = tm
        go k tm@(Var (Free occurname))
          | bindname == occurname = Var (Bound k ())
          | otherwise = tm
        go _ tm@(Var (Bound _ _)) = tm
        go k (t1 :@ t2) = go k t1 :@ go k t2
        go k (Abs scope) = Abs (goScope k scope)

        goScope :: Int -> TermScope -> TermScope
        goScope k (ManualScope name inner) = ManualScope name (go (k + 1) inner)

        (prettyname, bindty) = nameInfo bindname

openTerm :: TermScope -> Term -> Term
openTerm (ManualScope _ body) sub = go 0 body
  where go :: Int -> Term -> Term
        go _ tm@(Const _) = tm
        go _ tm@(Meta _) = tm
        go _ tm@(Var (Free _)) = tm
        go k tm@(Var (Bound j ()))
          | k == j = sub
          | otherwise = tm
        go k (t1 :@ t2) = go k t1 :@ go k t2
        go k (Abs scope) = Abs (goScope k scope)

        goScope :: Int -> TermScope -> TermScope
        goScope k (ManualScope name inner) = ManualScope name (go (k + 1) inner)

unbindTerm :: MonadFresh m => TermScope -> m (TermName, Term)
unbindTerm scope = do
  freshname <- scopeName scope
  return (freshname, openTerm scope (Var (Free freshname)))

substTerm :: Term -> Term -> Term -> Term
substTerm source target = go
  where go :: Term -> Term
        go tm
          | tm == source = target

        go tm@(Const _) = tm
        go tm@(Meta _) = tm
        go tm@(Var _) = tm

        go (t1 :@ t2) = go t1 :@ go t2
        go (Abs scope) = Abs (goScope scope)

        goScope :: TermScope -> TermScope
        goScope (ManualScope name inner) = ManualScope name (go inner)

substMeta :: MetaVar -> Term -> Term -> Term
substMeta metaId = substTerm (Meta metaId)

substVar :: TermName -> Term -> Term -> Term
substVar varName = substTerm (Var (Free varName))

metaVars :: Term -> Set MetaVar
metaVars (t1 :@ t2) = metaVars t1 `S.union` metaVars t2
metaVars (Abs (ManualScope _ body)) = metaVars body
metaVars (Meta m) = S.singleton m
metaVars (Var _) = S.empty
metaVars (Const _) = S.empty

scopeName :: MonadFresh m => TermScope -> m TermName
scopeName (ManualScope (Ignore name, ty) _) = fresh (name, ty)

resultType ::Ty -> Ty
resultType (_ :-> r) = resultType r
resultType ty = ty

argTypes :: Ty -> [Ty]
argTypes (ty1 :-> ty2) = ty1 : argTypes ty2
argTypes _ = []

createArrowType :: [Ty] -> Ty -> Ty
createArrowType argtys resty = foldr (:->) resty argtys

type Binders = [TermName]
type Spine = (Term, [Term])

collectLambdas :: MonadFresh m => Term -> m (Binders, Term)
collectLambdas (Abs scope) = do
  (x, body) <- unbindTerm scope
  (rest, inner) <- collectLambdas body
  return (x:rest, inner)
collectLambdas tm = return ([], tm)

createLambdas :: Binders -> Term -> Term
createLambdas binders tm = foldr addBinder tm binders
  where addBinder bindname inner = Abs (bindTerm bindname inner)

collectSpine :: Term -> Spine
collectSpine t = go t []
  where go (f :@ arg) acc = go f (arg:acc)
        go f acc = (f, acc)

createSpine :: Term -> [Term] -> Term
createSpine = foldl' (:@)
