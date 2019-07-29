{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module TypeCheck (doInferType, TC(..)) where

import Syntax
import Data.Text (Text)
import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.State.Strict
import Control.Monad.Reader
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

newtype TCContext = TCContext (Map TermName Ty)
newtype TCState = TCState { _varCount :: Int }

type TypeError = Text

newtype TC a = TC { runTC :: ReaderT TCContext (StateT TCState (ExceptT TypeError Identity)) a }
             deriving (Functor, Applicative, Monad)

instance MonadFresh TC where
  fresh (Name name _id) = TC $ do
    TCState count <- get
    put (TCState (count + 1))
    return (Name name count)

typeError :: Text -> TC a
typeError msg = TC (throwError msg)

withContext :: TermName -> Ty -> TC a -> TC a
withContext name ty (TC act) = TC $ local (\(TCContext ctxt) -> TCContext (M.insert name ty ctxt)) act

lookupType :: TermName -> TC Ty
lookupType name = TC $ do
  TCContext ctxt <- ask
  case M.lookup name ctxt of
    Just ty -> return ty
    Nothing -> error "name lookup failed"

checkEqual :: Ty -> Ty -> TC ()
checkEqual ty1 ty2
  | ty1 == ty2 = return ()
  | otherwise = typeError "type mismatch"

checkType :: Term -> Ty -> TC ()
checkType tm expectty = do
  actualty <- inferType tm
  checkEqual expectty actualty

inferType :: Term -> TC Ty
inferType (Const (ConstI _)) = return Int
inferType (Const (ConstB _)) = return Bool

inferType (Plus t1 t2) = do
  checkType t1 Int
  checkType t2 Int
  return Int

inferType (IfThenElse cond t1 t2) = do
  checkType cond Bool
  ty1 <- inferType t1
  ty2 <- inferType t2
  checkEqual ty1 ty2
  return ty1

inferType (Var (Free name)) = lookupType name
inferType (Var (Bound _ _)) = error "type checker encountered bound variable"

inferType (Abs ty scope) = do
  (x, body) <- unbindTerm scope
  withContext x ty (inferType body)

inferType (fun :@ arg) = do
  funty <- inferType fun
  actualty <- inferType arg
  case funty of
    argty :-> resultty -> do
      checkEqual argty actualty
      return resultty
    _ -> typeError "expected function type"

run :: TC a -> Either TypeError a
run act = runIdentity (runExceptT (evalStateT (runReaderT (runTC act) initialContext) initialState))
  where initialContext :: TCContext
        initialContext = TCContext M.empty

        initialState :: TCState
        initialState = TCState 0

doInferType :: Term -> Either TypeError Ty
doInferType tm = run (inferType tm)
