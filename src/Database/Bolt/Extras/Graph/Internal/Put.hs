{-# LANGUAGE DeriveGeneric     #-}
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

  , JNPutRequest (..)
  , JRPutRequest (..)
  , JGraphPutRequest
  , convertJNPutRequest
  , convertJRPutRequest
  ) where

import           Data.Aeson                                        (FromJSON (..),
                                                                    ToJSON (..),
                                                                    genericParseJSON,
                                                                    genericToJSON,
                                                                    omitNothingFields)
import           Data.Aeson.Casing                                 (aesonDrop,
                                                                    aesonPrefix,
                                                                    snakeCase)
import           Data.List                                         (foldl')
import           Data.Map.Strict                                   (Map, toList,
                                                                    (!))
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
import           Database.Bolt.Extras.Utils                        (dummyId)
import           GHC.Generics                                      (Generic)
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
--
requestPut :: [(NodeName, PutNode)]
           -> [((NodeName, NodeName), PutRelationship)]
           -> Text
requestPut pns prs = fst fullRequest
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
  return' (name, _) = [text|ID($name) AS $name|]

instance Returnable ((NodeName, NodeName), PutRelationship) where
  return' (names, _) = let name = relationName names
                         in [text|ID($name) AS $name|]

------------------------------------------------------------------------------------------------
-- | AESON FORMAT

-- | 'JNPutRequest' represents a 'Node' of the 'JGraphPutRequest'.
-- 'npreqUploadType' can be @"id"@, @"merge"@ or @"create"@.
-- If 'npreqUploadType':
--   * "merge"  - query with 'MERGE' will be performed,
--                fields 'npreqLabels' and 'npreqProps' should be presented;
--   * "create" - query with 'CREATE' will be performed,
--                fields 'npreqLabels' and 'npreqProps' should be presented;
--   * "id"     - no query will be performed,
--                it means you already know the 'BoltId' of this 'Node' in the database,
--                field 'npreqBoltId' should be presented.
--
data JNPutRequest = JNPutRequest { npreqUploadType :: Text
                                 , npreqBoltId     :: Maybe BoltId
                                 , npreqLabels     :: Maybe [Text]
                                 , npreqProps      :: Maybe (Map Text Value)
                                 }
  deriving (Show, Eq, Generic)

-- | 'JNPutRequest' represented a 'Relationship' of the 'JGraphPutRequest'.
--'rpreqUploadType' can be @"merge"@ or @"create"@.
-- If 'rpreqUploadType':
--   * "merge"  - query with 'MERGE' will be performed;
--   * "create" - query with 'CREATE' will be performed.
--
data JRPutRequest = JRPutRequest { rpreqUploadType :: Text
                                 , rpreqLabel      :: Text
                                 , rpreqProps      :: Map Text Value
                                 }
  deriving (Show, Eq, Generic)

convertJNPutRequest :: JNPutRequest -> PutNode
convertJNPutRequest (JNPutRequest "id" (Just boltId) Nothing Nothing)          = BoltId boltId
convertJNPutRequest (JNPutRequest "merge"  Nothing (Just labels) (Just props)) = MergeN  $ Node dummyId labels props
convertJNPutRequest (JNPutRequest "create" Nothing (Just labels) (Just props)) = CreateN $ Node dummyId labels props
convertJNPutRequest _ = error "Web.Semantic.Query.Converters: can't convert from JNPutRequest"

convertJRPutRequest :: JRPutRequest -> PutRelationship
convertJRPutRequest (JRPutRequest "merge"  label props) = MergeR  $ URelationship dummyId label props
convertJRPutRequest (JRPutRequest "create" label props) = CreateR $ URelationship dummyId label props
convertJRPutRequest _ = error "Web.Semantic.Query.Converters: can't convert from JRPutRequest"

instance Requestable (NodeName, JNPutRequest) where
  request (name, jpn) = request (name, convertJNPutRequest jpn)

instance Requestable ((NodeName, NodeName), JRPutRequest) where
  request (names, jpr) = request (names, convertJRPutRequest jpr)

instance Returnable (NodeName, JNPutRequest) where
  return' (name, _) = [text|ID($name) AS $name|]

instance Returnable ((NodeName, NodeName), JRPutRequest) where
  return' (names, _) = let name = relationName names
                         in [text|ID($name) AS $name|]

instance ToJSON JNPutRequest where
  toJSON = genericToJSON (aesonPrefix snakeCase)
    { omitNothingFields = True }
instance FromJSON JNPutRequest where
  parseJSON = genericParseJSON (aesonPrefix snakeCase)
    { omitNothingFields = True }

instance ToJSON JRPutRequest where
  toJSON = genericToJSON (aesonPrefix snakeCase)
    { omitNothingFields = True }
instance FromJSON JRPutRequest where
  parseJSON = genericParseJSON (aesonPrefix snakeCase)
    { omitNothingFields = True }

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

-- | JSON graph types

-- | 'JGraphPutRequest' represents a JSON graph, which we want to upload to the database.
--
type JGraphPutRequest = Graph NodeName JNPutRequest JRPutRequest

instance ToJSON JGraphPutRequest where
  toJSON = genericToJSON $ aesonDrop 1 snakeCase
instance FromJSON JGraphPutRequest where
  parseJSON = genericParseJSON $ aesonDrop 1 snakeCase

instance ToJSON GraphPutResponse where
  toJSON = genericToJSON $ aesonDrop 1 snakeCase
instance FromJSON GraphPutResponse where
  parseJSON = genericParseJSON $ aesonDrop 1 snakeCase
