{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module TypeCheck (doInferType, TC(..), run, inferType) where

import Syntax
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.State.Strict
import Control.Monad.Reader
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

newtype TCState = TCState { _varCount :: Int }

type TypeError = Text

newtype TC a = TC { runTC :: StateT TCState (ExceptT TypeError Identity) a }
             deriving (Functor, Applicative, Monad)

instance MonadFresh TC where
  fresh (Name name _id) = TC $ do
    TCState count <- get
    put (TCState (count + 1))
    return (Name name count)

typeError :: Text -> TC a
typeError msg = TC (throwError msg)

checkEqual :: Ty -> Ty -> TC ()
checkEqual ty1 ty2
  | ty1 == ty2 = return ()
  | otherwise = typeError "type mismatch"

inferType :: Term -> TC Ty
inferType (Meta (MetaVar _ ty)) = return ty
inferType (Const (ConstI _)) = return Int
inferType (Const (ConstB _)) = return Bool
inferType (Const Plus) = return (Int :-> Int :-> Int)
inferType (Const (IfThenElse ty)) = return (Bool :-> ty :-> ty :-> ty)

inferType (Var (Free name)) = return $ snd (nameName name)
inferType (Var (Bound _ _)) = error "type checker encountered bound variable"

inferType (Abs scope) = do
  (x, body) <- unbindTerm scope
  let ty = snd (nameName x)
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

run :: TC a -> Either TypeError a
run act = runIdentity (runExceptT (evalStateT (runTC act) initialState))
  where initialState :: TCState
        initialState = TCState 0

doInferType :: Term -> Either TypeError Ty
doInferType tm = run (inferType tm)
