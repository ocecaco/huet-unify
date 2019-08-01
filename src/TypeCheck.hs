{-# LANGUAGE OverloadedStrings #-}
module TypeCheck
  ( inferType
  )
where

import Syntax
import SyntaxHelper
import TCMonad (TC, typeError)
import Name
import qualified Data.Text as T

checkEqual :: Ty -> Ty -> TC ()
checkEqual ty1 ty2
  | ty1 == ty2 = return ()
  | otherwise = typeError "type mismatch"

inferType :: Term -> TC Ty
inferType (Meta (MetaVar name)) = return (nameInfo name)
inferType (Const (ConstI _)) = return Int
inferType (Const (ConstB _)) = return Bool
inferType (Const Plus) = return (Int :-> Int :-> Int)
inferType (Const (IfThenElse ty)) = return (Bool :-> ty :-> ty :-> ty)

inferType (Var (Free name)) = return $ snd (nameInfo name)
inferType (Var (Bound _ _)) = error "type checker encountered bound variable"

inferType (Abs scope) = do
  (x, body) <- unbindTerm scope
  let ty = snd (nameInfo x)
  resty <- inferType body
  return (ty --> resty)

inferType (fun :@ arg) = do
  funty <- inferType fun
  actualty <- inferType arg
  case funty of
    argty :-> resultty -> do
      checkEqual argty actualty
      return resultty
    _ -> typeError $ "expected function type, got " <> T.pack (show funty)
