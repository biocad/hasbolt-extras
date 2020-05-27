{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Database.Bolt.Extras.Graph.Internal.AbstractGraph
  (
    Graph (..)
  , vertices
  , relations
  , emptyGraph
  , addNode
  , addRelation
  , NodeName
  , relationName
  ) where

import           Control.Lens    (makeLenses, over)
import           Data.Map.Strict (Map, insert, notMember)
import           Data.Text       (Text)
import           GHC.Generics    (Generic)
import           Text.Printf     (printf)

-- | Representation of Graph that is used for requests and responses. It is parameterized by three types:
--
--   * @n@: type of node names
--   * @a@: type of nodes
--   * @b@: type of relations
--
-- Relations are described by a pair of nodes - start and end.
--
-- There are lenses defined for 'Graph': 'vertices' and 'relations'.
--
data Graph n a b = Graph { _vertices  :: Map n a
                         , _relations :: Map (n, n) b
                         } deriving (Show, Generic)

makeLenses ''Graph

-- | An empty graph.
--
emptyGraph :: Ord n => Graph n a b
emptyGraph = Graph mempty mempty

-- | Adds node to graph by its name and data.
-- If graph already contains node with given @name@, @error@ will be thrown.
--
addNode :: (Show n, Ord n)
        => n -- ^ Name of the node
        -> a -- ^ Node data
        -> Graph n a b -> Graph n a b
addNode name node graph = if name `notMember` _vertices graph
                          then over vertices (insert name node) graph
                          else error . printf "vertex with name %s key already exists" . show $ name

-- | Adds relation to graph by @startName@ of node, @endName@ of node, and @rel@ with relation data.
-- If graph already contains relation with given @(startName, endName)@, @error@ will be thrown.
--
addRelation :: (Show n, Ord n)
            => n -- ^ Name of start node
            -> n -- ^ Name of end node
            -> b -- ^ Relation data
            -> Graph n a b -> Graph n a b
addRelation startName endName rel graph = if (startName, endName) `notMember` _relations graph
                                          then over relations (insert (startName, endName) rel) graph
                                          else error $ printf "relation with names (%s, %s) already exists" (show startName) (show endName)

-- | Alias for text node name.
--
type NodeName = Text

-- | Build relationship name from the names of its start and end nodes
-- like @[startNodeName]0[endNodeName]@.
relationName :: (NodeName, NodeName) -> Text
relationName (st, en) = st <> "0" <> en
