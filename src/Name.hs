module Name
  ( Name
  , nameInfo
  , manualName
  , Var(..)
  , Scope(..)
  , Ignore(..)
  , MonadFresh(..)
  , freshFromName
  )
where

import Data.Text (Text)

data Name a = Name { nameInfo :: a, nameId :: Int, nameTag :: Text }
            deriving (Eq, Ord, Show)

manualName :: a -> Int -> Text -> Name a
manualName = Name

data Var f b = Free (Name f)
             | Bound Int b
             deriving (Eq, Ord, Show)

data Scope p t = ManualScope p t
               deriving (Eq, Ord, Show)

newtype Ignore a = Ignore a
              deriving (Show)

instance Eq (Ignore a) where
  _ == _ = True

instance Ord (Ignore a) where
  _ `compare` _ = EQ

class Monad m => MonadFresh m where
  fresh :: f -> m (Name f)

freshFromName :: MonadFresh m => Name f -> m (Name f)
freshFromName name = fresh (nameInfo name)
