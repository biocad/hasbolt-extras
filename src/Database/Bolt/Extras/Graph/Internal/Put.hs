{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Database.Bolt.Extras.Graph.Internal.Put
  (
    PutNode (..)
  , PutRelationship (..)
  , GraphPutRequest
  , GraphPutResponse
  , requestPut
  ) where

import           Data.List                                         (foldl')
import           Data.Map.Strict                                   (toList, (!))
import           Data.Monoid                                       ((<>))
import           Data.Text                                         (Text,
                                                                    intercalate,
                                                                    pack)
import           Database.Bolt                                     (Node (..), RecordValue (..),
                                                                    URelationship (..),
                                                                    Value (..),
                                                                    exact)
import           Database.Bolt.Extras                              (BoltId, ToCypher (..),
                                                                    fromInt)
import           Database.Bolt.Extras.Graph.Internal.AbstractGraph (Graph (..),
                                                                    NodeName,
                                                                    relationName)
import           Database.Bolt.Extras.Graph.Internal.Class         (Extractable (..),
                                                                    Requestable (..),
                                                                    Returnable (..))
import           NeatInterpolation                                 (text)

------------------------------------------------------------------------------------------------
-- REQUEST --
------------------------------------------------------------------------------------------------
-- | BOLT FORMAT

-- | 'PutNode' is the wrapper for 'Node' where we can specify if we want to merge or create it.
--
data PutNode = BoltId BoltId | MergeN Node | CreateN Node
  deriving (Show)

-- | 'PutRelationship' is the wrapper for 'Relationship' where we can specify if we want to merge or create it.
--
data PutRelationship = MergeR URelationship | CreateR URelationship
  deriving (Show)

instance Requestable (NodeName, PutNode) where
  request (name, BoltId boltId) = let showBoltId = pack . show $ boltId
                                  in [text|MATCH ($name) WHERE ID($name) = $showBoltId|]
  request (name, MergeN node)   = requestNode "MERGE"  name node
  request (name, CreateN node)  = requestNode "CREATE" name node

requestNode :: Text -> NodeName -> Node -> Text
requestNode q name Node{..} = [text|$q ($name $labelsQ {$propsQ})|]
  where
    labelsQ = toCypher labels
    propsQ  = toCypher . filter ((/= N ()) . snd) . toList $ nodeProps

instance Requestable ((NodeName, NodeName), PutRelationship) where
  request (names, MergeR urel)  = requestURelationship "MERGE" names urel
  request (names, CreateR urel) = requestURelationship "CREATE" names urel

requestURelationship :: Text -> (NodeName, NodeName) -> URelationship -> Text
requestURelationship q (stName, enName) URelationship{..} =
    [text|$q ($stName)-[$name $labelQ {$propsQ}]->($enName)|]
  where
    name   = relationName (stName, enName)
    labelQ = toCypher urelType
    propsQ = toCypher . toList $ urelProps

-- | Takes all 'PutNode's and 'PutRelationship's
-- and write them to single query to request.
-- Here "WITH" is used, because you cannot perform
-- "match", "merge" or "create" at the same query.
requestPut :: [(NodeName, PutNode)]
           -> [((NodeName, NodeName), PutRelationship)]
           -> (Text, [Text])
requestPut pns prs = (fst fullRequest, [])
  where
    foldStepN :: (Text, [NodeName]) -> (NodeName, PutNode) -> (Text, [NodeName])
    foldStepN accum pn@(name, _) = foldStep accum name pn

    foldStepR :: (Text, [NodeName]) -> ((NodeName, NodeName), PutRelationship) -> (Text, [NodeName])
    foldStepR accum pr@(names, _) = foldStep accum (relationName names) pr

    foldStep :: Requestable a => (Text, [NodeName]) -> NodeName -> a -> (Text, [NodeName])
    foldStep (currentQuery, names) name put =
        (currentQuery <> request put <> " WITH " <> intercalate ", " updNames <> " ", updNames)
      where
        updNames = name : names

    requestNodes = foldl' foldStepN ("", []) pns
    fullRequest  = foldl' foldStepR requestNodes prs

instance Returnable (NodeName, PutNode) where
  -- always return all nodes
  isReturned' _     = True
  return' (name, _) = [text|ID($name) AS $name|]

instance Returnable ((NodeName, NodeName), PutRelationship) where
  -- always return all relations
  isReturned' _      = True
  return' (names, _) = let name = relationName names
                       in [text|ID($name) AS $name|]

------------------------------------------------------------------------------------------------

----------------------------------------------------------
-- RESULT --
----------------------------------------------------------

instance Extractable BoltId where
  extract name = mapM (fmap fromInt . exact . (! name))

----------------------------------------------------------
-- GRAPH TYPES --
----------------------------------------------------------

-- | The graph of 'Node's with specified uploading type and 'URelationship's.
--
type GraphPutRequest = Graph NodeName PutNode PutRelationship

-- | The graph of 'BoltId's corresponding to the nodes and relationships
-- which we get after putting 'GraphPutRequest'.
--
type GraphPutResponse = Graph NodeName BoltId BoltId
