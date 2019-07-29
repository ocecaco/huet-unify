{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Normalize (doNormalizeTerm, doNormalizeTermWithEta) where

import Syntax
import TypeCheck
import Control.Monad
import Data.List
import Debug.Trace (traceM)

normalizeTerm :: Term -> TC Term
normalizeTerm c@(Const _) = return c
normalizeTerm tm@(Var (Free _)) = return tm

normalizeTerm (Var (Bound _ _)) = error "normalization encountered bound variable"

normalizeTerm (t1 :@ t2) = do
  normt1 <- normalizeTerm t1
  normt2 <- normalizeTerm t2
  case normt1 :@ normt2 of
    Abs _ scope :@ arg -> normalizeTerm (openTerm scope arg)
    Const Plus :@ Const (ConstI n1) :@ Const (ConstI n2) -> return $ Const (ConstI (n1 + n2))
    Const (IfThenElse _) :@ Const (ConstB c) :@ success :@ failure ->
      if c then return success else return failure
    n -> return n

normalizeTerm (Abs ty scope) = do
  (x, body) <- unbindTerm scope
  normbody <- normalizeTerm body
  return (Abs ty (bindTerm x normbody))

-- Eta-expands a term, which is assumed to be already in beta-normal
-- form
etaExpand :: Term -> TC Term
etaExpand tm = do
  ty <- inferType tm
  let argTypes = collectArgTypes ty
  (binders, (hd, args)) <- collectLambdas tm
  argsExpanded <- withContextTelescope binders $ mapM etaExpand args

  let missingBinderTypes = drop (length binders) argTypes
  missingBinderNames <- replicateM (length missingBinderTypes) (freshFromRawName "eta")
  let missingBinderVars = map (Var . Free) missingBinderNames
  let missingBinders = zip missingBinderNames missingBinderTypes

  let newArgs = argsExpanded ++ missingBinderVars
  let newBinders = binders ++ missingBinders

  let expanded = foldr addBinder (createSpine hd newArgs) newBinders
        where addBinder (bindname, bindty) inner = Abs bindty (bindTerm bindname inner)

  return expanded

collectArgTypes :: Ty -> [Ty]
collectArgTypes (ty1 :-> ty2) = ty1 : collectArgTypes ty2
collectArgTypes _ = []

type Binders = [(TermName, Ty)]
type Spine = (Term, [Term])

collectLambdas :: Term -> TC (Binders, Spine)
collectLambdas (Abs ty scope) = do
  (x, body) <- unbindTerm scope
  (rest, inner) <- collectLambdas body
  return ((x,ty):rest, inner)
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
