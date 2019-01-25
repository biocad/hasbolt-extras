module Database.Bolt.Extras.Graph.Internal.Class
  (
    Requestable (..)
  , Returnable (..)
  , Extractable (..)
  ) where

import           Control.Monad.IO.Class (MonadIO)
import           Data.Text              (Text)
import           Database.Bolt          (BoltActionT, Record)

-- | Class describes entity, which can be requested.
--
class Requestable a where
  -- | How to convert entity to Cypher.
  request         :: a -> Text

-- | Class describes entity, which can be returned.
--
class Returnable a where
  -- | If the entity should be returned.
  isReturned' :: a -> Bool

  -- | How to return entity in the Cypher.
  return' :: a -> Text

-- | Class describes entity, which can be extracted from records by name.
--
class Extractable a where
  extract :: MonadIO m => Text -> [Record] -> BoltActionT m [a]
