{-# LANGUAGE ExistentialQuantification #-}

module Database.Bolt.Extras.Selector.Types where

import           Data.Map.Strict                     (Map)
import           Data.Text                           (Text)
import           Database.Bolt                       as B (Value)
import           Database.Bolt.Extras                (BoltId, Label)

import           Database.Bolt.Extras.Selector.Class (Requestable)

-- | Helper to find 'Node's.
--
data NodeGetter = NodeGetter { ngboltId      :: Maybe BoltId     -- ^ known 'BoltId'
                             , ngLabels      :: [Label]          -- ^ known labels
                             , ngProps       :: Map Text B.Value -- ^ known properties
                             , ngReturnProps :: [Text]           -- ^ names of properties to return
                             , ngIsReturned  :: Bool             -- ^ whether to return this node or not
                             }
  deriving (Show, Eq)

-- | Helper to find 'URelationship's.
--
data RelGetter = RelGetter { rgboltId      :: Maybe BoltId     -- ^ known 'BoltId'
                           , rgLabel       :: Maybe Label      -- ^ known labels
                           , rgProps       :: Map Text B.Value -- ^ known properties
                           , rgReturnProps :: [Text]           -- ^ names of properties to return
                           , rgIsReturned  :: Bool             -- ^ whether to return this relation or not
                           }
  deriving (Show, Eq)

data Selector = forall a. Requestable a => Selector a

type Selectors = [Selector]

type NodeSelector = (NodeName, NodeGetter)
type RelSelector = (RelName, RelGetter)

-- | Alias for text node name.
--
type NodeName = Text
type RelName = Text
