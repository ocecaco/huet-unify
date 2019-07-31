{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Normalize (doNormalizeTerm, doNormalizeTermWithEta) where

import Syntax
import TypeCheck
import Data.List

normalizeTerm :: Term -> TC Term
normalizeTerm c@(Const _) = return c
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
  (binders, (hd, args)) <- collectLambdas tm

  let missingBinderTypes = drop (length binders) argTypes
  missingBinderNames <- mapM (\bindty -> freshFromRawName ("eta", bindty)) missingBinderTypes
  let missingBinderVars = map (Var . Free) missingBinderNames

  argsExpanded <- mapM etaExpand (args ++ missingBinderVars)
  let newBinders = binders ++ missingBinderNames

  let expanded = foldr addBinder (createSpine hd argsExpanded) newBinders
        where addBinder bindname inner = Abs (bindTerm bindname inner)

  return expanded

collectArgTypes :: Ty -> [Ty]
collectArgTypes (ty1 :-> ty2) = ty1 : collectArgTypes ty2
collectArgTypes _ = []

type Binders = [TermName]
type Spine = (Term, [Term])

collectLambdas :: Term -> TC (Binders, Spine)
collectLambdas (Abs scope) = do
  (x, body) <- unbindTerm scope
  (rest, inner) <- collectLambdas body
  return (x:rest, inner)
collectLambdas tm = return ([], collectSpine tm)

collectSpine :: Term -> (Term, [Term])
collectSpine t = go t []
  where go (f :@ arg) acc = go f (arg:acc)
        go f acc = (f, acc)

createSpine :: Term -> [Term] -> Term
createSpine = foldl' (:@)

doNormalizeTerm :: Term -> Term
doNormalizeTerm tm = case run (normalizeTerm tm) of
  Left _ -> error "unexpected type error while normalizing"
  Right t -> t

doNormalizeTermWithEta :: Term -> Term
doNormalizeTermWithEta tm = case run (normalizeTerm tm >>= etaExpand) of
  Left _ -> error "unexpected type error while normalizing"
  Right t -> t
