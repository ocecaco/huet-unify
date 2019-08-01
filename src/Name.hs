module Name
  ( Name(..)
  , Var(..)
  , Scope(..)
  , Ignore(..)
  , MonadFresh(..)
  , freshFromRawName
  )
where

data Name a = Name { nameName :: a, nameUniqueId :: Int }
            deriving (Eq, Ord, Show)

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

freshFromRawName :: MonadFresh m => f -> m (Name f)
freshFromRawName raw = fresh (Name raw 0)
