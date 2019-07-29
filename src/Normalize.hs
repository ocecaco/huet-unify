{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Normalize (doNormalizeTerm) where

import Syntax
import TypeCheck

normalizeTerm :: Term -> TC Term
normalizeTerm c@(Const _) = return c
normalizeTerm tm@(Var (Free _)) = return tm

normalizeTerm (Var (Bound _ _)) = error "normalization encountered bound variable"

normalizeTerm (t1 :@ t2) = do
  normt1 <- normalizeTerm t1
  normt2 <- normalizeTerm t2
  case normt1 of
    Abs _ scope -> normalizeTerm (openTerm scope normt2)
    _ -> return (normt1 :@ normt2)

normalizeTerm (Abs ty scope) = do
  (x, body) <- unbindTerm scope
  normbody <- normalizeTerm body
  return (Abs ty (bindTerm x normbody))

normalizeTerm (Plus t1 t2) = do
  normt1 <- normalizeTerm t1
  normt2 <- normalizeTerm t2
  case (normt1, normt2) of
    (Const (ConstI n1), Const (ConstI n2)) -> return (Const (ConstI (n1 + n2)))
    _ -> error "normalizer expected numbers"

normalizeTerm (IfThenElse cond t1 t2) = do
  normcond <- normalizeTerm cond
  case normcond of
    Const (ConstB True) -> normalizeTerm t1
    Const (ConstB False) -> normalizeTerm t2
    _ -> error "normalizer expected boolean"

doNormalizeTerm :: Term -> Term
doNormalizeTerm tm = case run (normalizeTerm tm) of
  Left _ -> error "unexpected type error while normalizing"
  Right t -> t
