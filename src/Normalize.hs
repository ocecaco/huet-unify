{-# LANGUAGE OverloadedStrings #-}
module Normalize (normalize, etaExpand, normalizeEta) where

import Syntax
import TypeCheck
import Name
import TCMonad

normalize :: Term -> TC Term
normalize c@(Const _) = return c
normalize m@(Meta _) = return m
normalize tm@(Var (Free _)) = return tm

normalize (Var (Bound _ _)) = error "normalization encountered bound variable"

normalize (t1 :@ t2) = do
  normt1 <- normalize t1
  normt2 <- normalize t2
  case normt1 :@ normt2 of
    Abs scope :@ arg -> normalize (openTerm scope arg)
    Const Plus :@ Const (ConstI n1) :@ Const (ConstI n2) -> return $ Const (ConstI (n1 + n2))
    Const (IfThenElse _) :@ Const (ConstB c) :@ success :@ failure ->
      if c then return success else return failure
    n -> return n

normalize (Abs scope) = do
  (x, body) <- unbindTerm scope
  normbody <- normalize body
  return (Abs (bindTerm x normbody))

normalizeEta :: Term -> TC Term
normalizeEta tm = normalize tm >>= etaExpand

-- Eta-expands a term, which is assumed to be already in beta-normal
-- form
etaExpand :: Term -> TC Term
etaExpand tm = do
  ty <- inferType tm
  let argTys = argTypes ty
  (binders, body) <- collectLambdas tm
  let (hd, args) = collectSpine body

  let missingBinderTypes = drop (length binders) argTys
  missingBinderNames <- mapM (\bindty -> freshFromNameInfo ("eta", bindty)) missingBinderTypes
  let missingBinderVars = map (Var . Free) missingBinderNames

  argsExpanded <- mapM etaExpand (args ++ missingBinderVars)
  let newBinders = binders ++ missingBinderNames

  let expanded = createLambdas newBinders (createSpine hd argsExpanded)
  return expanded
