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
  -- | Condition for BoltId like "ID(a) = b" if BoltId is presented.
  maybeBoltIdCond :: a -> Maybe Text
  -- | How to convert entity to Cypher.
  request         :: a -> Text

-- | Class describes entity, which can be returned.
--
class Returnable a where
  -- | How to return entity in the Cypher.
  return' :: a -> Text

-- | Class describes entity, which can be extracted from records by name.
--
class Extractable a where
  extract :: MonadIO m => Text -> [Record] -> BoltActionT m [a]
