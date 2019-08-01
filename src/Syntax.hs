module Syntax where

import Data.Text (Text)

data Name a = Name { nameName :: a, nameUniqueId :: Int }
            deriving (Eq, Ord, Show)

data Var f b = Free (Name f)
             | Bound Int b
             deriving (Eq, Ord, Show)

data Scope p t = Scope p t
               deriving (Eq, Ord, Show)

newtype Ignore a = Ignore a
              deriving (Show)

instance Eq (Ignore a) where
  _ == _ = True

instance Ord (Ignore a) where
  _ `compare` _ = EQ

class Monad m => MonadFresh m where
  fresh :: Name f -> m (Name f)

freshFromRawName :: MonadFresh m => f -> m (Name f)
freshFromRawName raw = fresh (Name raw 0)

type TermName = Name (Text, Ty)
type TermVar = Var (Text, Ty) ()
type TermScope = Scope (Ignore Text, Ty) Term

data MetaVar = MetaVar Int Ty
             deriving (Eq, Ord, Show)

data Const = ConstI Int
           | ConstB Bool
           | Plus
           | IfThenElse Ty
           deriving (Eq, Ord, Show)

data Term = Term :@ Term -- application
          | Abs TermScope -- lambda-abstraction
          | Var TermVar -- free/bound variable
          | Const Const -- constants
          | Meta MetaVar -- metavariables
          deriving (Eq, Ord, Show)

data Ty = Ty :-> Ty
        | Int
        | Bool
        deriving (Eq, Ord, Show)

infixr :->
infixl :@

bindTerm :: TermName -> Term -> TermScope
bindTerm bindname bindterm = Scope (Ignore . fst . nameName $ bindname, snd . nameName $ bindname) (go 0 bindterm)
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
        goScope k (Scope name inner) = Scope name (go (k + 1) inner)

openTerm :: TermScope -> Term -> Term
openTerm (Scope _ body) sub = go 0 body
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
        goScope k (Scope name inner) = Scope name (go (k + 1) inner)

unbindTerm :: MonadFresh m => TermScope -> m (TermName, Term)
unbindTerm scope@(Scope (Ignore origname, ty) _) = do
  freshname <- freshFromRawName (origname, ty)
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
        goScope (Scope name inner) = Scope name (go inner)

substMeta :: MetaVar -> Term -> Term -> Term
substMeta metaId = substTerm (Meta metaId)

substVar :: TermName -> Term -> Term -> Term
substVar varName = substTerm (Var (Free varName))

scopeName :: MonadFresh m => TermScope -> m TermName
scopeName (Scope (Ignore name, ty) _) = freshFromRawName (name, ty)

lam :: Text -> Ty -> (Term -> Term) -> Term
lam name ty body = Abs bodyScope
  where typedName = Name (name, ty) 0
        typedVar = Var (Free typedName)
        bodyWithVar = body typedVar
        bodyScope = bindTerm typedName bodyWithVar

(@@) :: Term -> Term -> Term
t1 @@ t2 = t1 :@ t2

i :: Int -> Term
i nv = Const (ConstI nv)

b :: Bool -> Term
b bv = Const (ConstB bv)

ifthenelse :: Ty -> Term -> Term -> Term -> Term
ifthenelse ty cond t1 t2 = Const (IfThenElse ty) @@ cond @@ t1 @@ t2

(+.) :: Term -> Term -> Term
t1 +. t2 = Const Plus @@ t1 @@ t2

int :: Ty
int = Int

bool :: Ty
bool = Bool

(-->) :: Ty -> Ty -> Ty
ty1 --> ty2 = ty1 :-> ty2

infixl @@
infixr -->
