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
import           Data.Monoid     ((<>))
import           Data.Text       (Text)
import           GHC.Generics    (Generic)
import           Text.Printf     (printf)

-- | 'Graph' contains vertices, that are parameterized by some type @n@, and relations,
-- that parameterized by pair of type @n@. This pair represents vertices, that are connected with this relation.
--
data Graph n a b = Graph { _vertices  :: Map n a
                         , _relations :: Map (n, n) b
                         } deriving (Show, Generic)

makeLenses ''Graph

-- | Creates empty graph.
--
emptyGraph :: Ord n => Graph n a b
emptyGraph = Graph mempty mempty

-- | Adds node to graph by it's @name@ and @node@ content.
-- If graph already contains vertex with given @name@, error will be thrown.
--
addNode :: (Show n, Ord n) => n -> a -> Graph n a b -> Graph n a b
addNode name node graph = if name `notMember` _vertices graph
                          then over vertices (insert name node) graph
                          else error . printf "vertex with name %s key already exists" . show $ name

-- | Adds relation to graph by @startName@ of vertex, @endName@ of vertex, and @rel@ with relation content.
-- If graph already contains relation with given @(startName, endName)@, error will be thrown.
--
addRelation :: (Show n, Ord n) => n -> n -> b -> Graph n a b -> Graph n a b
addRelation startName endName rel graph = if (startName, endName) `notMember` _relations graph
                                          then over relations (insert (startName, endName) rel) graph
                                          else error $ printf "relation with names (%s, %s) already exists" (show startName) (show endName)

-- | Alias for text node name.
--
type NodeName = Text

-- | Creates relationship name from the names of its start and end nodes
-- in the way `<startNodeName>0<endNodeName>`.
relationName :: (NodeName, NodeName) -> Text
relationName (st, en) = st <> "0" <> en
