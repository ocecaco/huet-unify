{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module TCMonad
  ( TC
  , run
  , TypeError
  , typeError
  )
where

import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.State.Strict
import Control.Monad.Logic
import Control.Applicative (Alternative)
import Data.Text (Text)
import Name

newtype TCState = TCState { _varCount :: Int }

type TypeError = Text

newtype TC a = TC { runTC :: LogicT (StateT TCState (ExceptT TypeError Identity)) a }
             deriving (Functor, Applicative, Monad, Alternative, MonadPlus, MonadLogic)

instance MonadFresh TC where
  fresh name = TC $ do
    TCState count <- get
    put (TCState (count + 1))
    return (manualName (nameInfo name) count)

typeError :: Text -> TC a
typeError msg = TC (throwError msg)

run :: TC a -> Either TypeError a
run act = runIdentity (runExceptT (evalStateT (observeT (runTC act)) initialState))
  where initialState :: TCState
        initialState = TCState 0
