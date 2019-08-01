module SyntaxHelper
  ( lam
  , (@@)
  , meta
  , i
  , b
  , ifthenelse
  , (+.)
  , int
  , bool
  , (-->)
  )
where

import Data.Text (Text)
import Syntax
import Name

lam :: Text -> Ty -> (Term -> Term) -> Term
lam name ty body = Abs bodyScope
  where typedName = manualName (name, ty) 0
        typedVar = Var (Free typedName)
        bodyWithVar = body typedVar
        bodyScope = bindTerm typedName bodyWithVar

(@@) :: Term -> Term -> Term
t1 @@ t2 = t1 :@ t2

meta :: Int -> Ty -> Term
meta n ty = Meta (MetaVar (manualName ty n))

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
