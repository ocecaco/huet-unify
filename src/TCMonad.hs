{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module TCMonad
  ( TC
  , runTC
  , TypeError
  , typeError
  )
where

import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.State.Strict
import Data.Text (Text)
import Name

newtype TCState = TCState { _varCount :: Int }

type TypeError = Text

newtype TC a = TC { unTC :: StateT TCState (ExceptT TypeError Identity) a }
             deriving (Functor, Applicative, Monad)

instance MonadFresh TC where
  fresh name = TC $ do
    TCState count <- get
    put (TCState (count + 1))
    return (manualName name count "typechecker")

typeError :: Text -> TC a
typeError msg = TC (throwError msg)

runTC :: TC a -> Either TypeError a
runTC act = runIdentity (runExceptT (evalStateT (unTC act) initialState))
  where initialState :: TCState
        initialState = TCState 0
