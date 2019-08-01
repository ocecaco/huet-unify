module Name
  ( Name
  , nameInfo
  , manualName
  , Var(..)
  , Scope(..)
  , Ignore(..)
  , MonadFresh(..)
  , freshFromNameInfo
  )
where

data Name a = Name a Int
            deriving (Eq, Ord, Show)

nameInfo :: Name a -> a
nameInfo (Name raw _) = raw

manualName :: a -> Int -> Name a
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
  fresh :: Name f -> m (Name f)

freshFromNameInfo :: MonadFresh m => f -> m (Name f)
freshFromNameInfo raw = fresh (Name raw 0)
