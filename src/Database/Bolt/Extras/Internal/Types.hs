module Database.Bolt.Extras.Internal.Types
  (
    FromValue (..)
  , Label
  , Labels (..)
  , NodeLike (..)
  , Properties (..)
  , Property
  , ToValue (..)
  , URelationLike (..)
  , ValueWrapper (..)
  ) where

import           GHC.Stack       (HasCallStack)
import           Data.Map.Strict (Map)
import           Data.Text       (Text)
import           Database.Bolt   (Node (..), URelationship (..), Value (..))

-- | Alias for Neo4j label.
--
type Label = Text

-- | Alias for Neo4j property.
--
type Property = (Text, Value)

-- | 'NodeLike' class represents convertable into and from 'Node'.
--
class NodeLike a where
  toNode :: HasCallStack => a -> Node
  fromNode :: HasCallStack => Node -> a

-- | 'URelationLike' class represents convertable into and from 'URelationship'.
--
class URelationLike a where
  toURelation :: HasCallStack => a -> URelationship
  fromURelation :: HasCallStack => URelationship -> a

-- | 'ToValue' means that something can be converted into Bolt 'Value'.
--
class ToValue a where
  toValue :: HasCallStack => a -> Value

-- | 'FromValue' means that something can be converted from Bolt 'Value'.
--
class FromValue a where
  fromValue :: HasCallStack => Value -> a

-- | 'Labels' means that labels can be obtained from entity.
--
class Labels a where
  getLabels :: HasCallStack => a -> [Label]

instance Labels Node where
  getLabels = labels

instance Labels URelationship where
  getLabels = pure . urelType

-- | 'Properties' means that properties can be obtained from entity.
--
class Properties a where
  getProps :: HasCallStack => a -> Map Text Value

instance Properties Node where
  getProps = nodeProps

instance Properties URelationship where
  getProps = urelProps

newtype ValueWrapper a = ValueWrapper a
