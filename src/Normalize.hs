{-# LANGUAGE OverloadedStrings #-}
module Normalize (normalizeTerm, etaExpand) where

import Syntax
import TypeCheck
import Name
import TCMonad

normalizeTerm :: Term -> TC Term
normalizeTerm c@(Const _) = return c
normalizeTerm m@(Meta _) = return m
normalizeTerm tm@(Var (Free _)) = return tm

normalizeTerm (Var (Bound _ _)) = error "normalization encountered bound variable"

normalizeTerm (t1 :@ t2) = do
  normt1 <- normalizeTerm t1
  normt2 <- normalizeTerm t2
  case normt1 :@ normt2 of
    Abs scope :@ arg -> normalizeTerm (openTerm scope arg)
    Const Plus :@ Const (ConstI n1) :@ Const (ConstI n2) -> return $ Const (ConstI (n1 + n2))
    Const (IfThenElse _) :@ Const (ConstB c) :@ success :@ failure ->
      if c then return success else return failure
    n -> return n

normalizeTerm (Abs scope) = do
  (x, body) <- unbindTerm scope
  normbody <- normalizeTerm body
  return (Abs (bindTerm x normbody))

-- Eta-expands a term, which is assumed to be already in beta-normal
-- form
etaExpand :: Term -> TC Term
etaExpand tm = do
  ty <- inferType tm
  let argTypes = collectArgTypes ty
  (binders, body) <- collectLambdas tm
  let (hd, args) = collectSpine body

  let missingBinderTypes = drop (length binders) argTypes
  missingBinderNames <- mapM (\bindty -> freshFromNameInfo ("eta", bindty)) missingBinderTypes
  let missingBinderVars = map (Var . Free) missingBinderNames

  argsExpanded <- mapM etaExpand (args ++ missingBinderVars)
  let newBinders = binders ++ missingBinderNames

  let expanded = foldr addBinder (createSpine hd argsExpanded) newBinders
        where addBinder bindname inner = Abs (bindTerm bindname inner)

  return expanded
