module Database.Bolt.Extras.Graph.Internal.Class
  ( Returnable (..)
  , Extractable (..)
  ) where

import           Control.Monad.IO.Class (MonadIO)
import           Data.Text              (Text)
import           Database.Bolt          (BoltActionT, Record)

-- | Entity  which can be returned from Neo4j in @RETURN@ operator.
--
class Returnable a where
  -- | If the entity should be returned.
  isReturned' :: a -> Bool

  -- | How to return entity in the Cypher.
  return' :: a -> Text

-- | Entity which can be extracted from 'Record' by its name.
--
class Extractable a where
  extract :: MonadIO m => Text -> [Record] -> BoltActionT m [a]
