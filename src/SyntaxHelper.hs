{-# LANGUAGE OverloadedStrings #-}
module SyntaxHelper
  ( lam
  , (@@)
  , meta
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

(-->) :: Ty -> Ty -> Ty
ty1 --> ty2 = ty1 :-> ty2

infixl @@
infixr -->
