{-# LANGUAGE OverloadedStrings #-}

module Database.Bolt.Extras.Selector.Defaults where

import           Database.Bolt.Extras.Selector.Types (NodeGetter (..),
                                                      RelGetter (..),
                                                      RelSelector)

-- | 'NodeGetter' that matches any node.
defaultNode :: Bool       -- ^ Whether to return the node
            -> NodeGetter
defaultNode = NodeGetter Nothing [] mempty []

-- | 'RelGetter' that matches any relation.
defaultRel :: Bool      -- ^ Whether to return the relation
           -> RelGetter
defaultRel = RelGetter Nothing Nothing mempty []

-- | 'NodeGetter' that matches any node and returns it.
defaultNodeReturn :: NodeGetter
defaultNodeReturn = defaultNode True

-- | 'NodeGetter' that matches any node and does not return it.
defaultNodeNotReturn :: NodeGetter
defaultNodeNotReturn = defaultNode False

-- | 'RelGetter' that matches any relation and returns it.
defaultRelReturn :: RelGetter
defaultRelReturn = defaultRel True


-- | 'RelGetter' that matches any relation and does not return it.
defaultRelNotReturn :: RelGetter
defaultRelNotReturn = defaultRel False

dr :: RelSelector
dr = ("", defaultRelNotReturn)
