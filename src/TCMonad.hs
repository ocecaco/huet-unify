{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module TCMonad
  ( TC
  , runTC
  , typeError
  , TCError(..)
  )
where

import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.State.Strict
import Data.Text (Text)
import Name

newtype TCState = TCState { _varCount :: Int }

data TCError = TypeError Text
             | ScopeError Text

newtype TC a = TC { unTC :: StateT TCState (ExceptT TCError Identity) a }
             deriving (Functor, Applicative, Monad)

instance MonadFresh TC where
  fresh name = TC $ do
    TCState count <- get
    put (TCState (count + 1))
    return (manualName name count)

typeError :: TCError -> TC a
typeError msg = TC (throwError msg)

runTC :: TC a -> Either TCError a
runTC act = runIdentity (runExceptT (evalStateT (unTC act) initialState))
  where initialState :: TCState
        initialState = TCState 0
