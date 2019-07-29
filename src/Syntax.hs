module Syntax where

import Data.Text (Text)

data Name a = Name { nameName :: a, nameUniqueId :: Int }
            deriving (Eq, Ord, Show)

data Var f b = Free (Name f)
             | Bound Int b

data Scope p t = Scope p t

newtype Ignore a = Ignore a
              deriving (Show)

instance Eq (Ignore a) where
  _ == _ = True

instance Ord (Ignore a) where
  _ `compare` _ = EQ

class Monad m => MonadFresh m where
  fresh :: Name f -> m (Name f)

type TermName = Name Text
type TermVar = Var Text ()
type TermScope = Scope (Ignore TermName) Term

data Const = ConstI Int
           | ConstB Bool

data Term = Term :@ Term -- application
          | Abs Ty TermScope -- lambda-abstraction
          | Var TermVar -- free/bound variable
          | Const Const -- constants
          | Plus Term Term -- addition
          | IfThenElse Term Term Term -- if-then-else conditional

data Ty = Ty :-> Ty
        | Int
        | Bool
        deriving (Eq, Ord, Show)

infixr :->
infixl :@

bindTerm :: TermName -> Term -> TermScope
bindTerm bindname bindterm = Scope (Ignore bindname) (go 0 bindterm)
  where go :: Int -> Term -> Term
        go _ tm@(Const _) = tm
        go k (Plus t1 t2) = Plus (go k t1) (go k t2)
        go k (IfThenElse cond t1 t2) = IfThenElse (go k cond) (go k t1) (go k t2)
        go k tm@(Var (Free occurname))
          | bindname == occurname = Var (Bound k ())
          | otherwise = tm
        go _ tm@(Var (Bound _ _)) = tm
        go k (t1 :@ t2) = go k t1 :@ go k t2
        go k (Abs ty scope) = Abs ty (goScope k scope)

        goScope :: Int -> TermScope -> TermScope
        goScope k (Scope name inner) = Scope name (go (k + 1) inner)

openTerm :: TermScope -> Term -> Term
openTerm (Scope _ body) sub = go 0 body
  where go :: Int -> Term -> Term
        go _ tm@(Const _) = tm
        go k (Plus t1 t2) = Plus (go k t1) (go k t2)
        go k (IfThenElse cond t1 t2) = IfThenElse (go k cond) (go k t1) (go k t2)
        go _ tm@(Var (Free _)) = tm
        go k tm@(Var (Bound i ()))
          | k == i = sub
          | otherwise = tm
        go k (t1 :@ t2) = go k t1 :@ go k t2
        go k (Abs ty scope) = Abs ty (goScope k scope)

        goScope :: Int -> TermScope -> TermScope
        goScope k (Scope name inner) = Scope name (go (k + 1) inner)

unbindTerm :: MonadFresh m => TermScope -> m (TermName, Term)
unbindTerm scope@(Scope (Ignore origname) _) = do
  freshname <- fresh origname
  return (freshname, openTerm scope (Var (Free freshname)))

substTerm :: TermName -> Term -> Term -> Term
substTerm subname sub = go
  where go :: Term -> Term
        go tm@(Const _) = tm
        go (Plus t1 t2) = Plus (go t1) (go t2)
        go (IfThenElse cond t1 t2) = IfThenElse (go cond) (go t1) (go t2)
        go tm@(Var (Free occurname))
          | subname == occurname = sub
          | otherwise = tm

        go tm@(Var (Bound _ _)) = tm
        go (t1 :@ t2) = go t1 :@ go t2
        go (Abs ty scope) = Abs ty (goScope scope)


        goScope :: TermScope -> TermScope
        goScope (Scope name inner) = Scope name (go inner)

lam :: Text -> Ty -> Term -> Term
lam name ty body = Abs ty (bindTerm (Name name 0) body)

(@@) :: Term -> Term -> Term
t1 @@ t2 = t1 :@ t2

v :: Text -> Term
v name = Var (Free (Name name 0))

i :: Int -> Term
i nv = Const (ConstI nv)

b :: Bool -> Term
b bv = Const (ConstB bv)

ifthenelse :: Term -> Term -> Term -> Term
ifthenelse = IfThenElse

(+.) :: Term -> Term -> Term
t1 +. t2 = Plus t1 t2

int :: Ty
int = Int

bool :: Ty
bool = Bool

(-->) :: Ty -> Ty -> Ty
ty1 --> ty2 = ty1 :-> ty2

infixl @@
infixr -->
