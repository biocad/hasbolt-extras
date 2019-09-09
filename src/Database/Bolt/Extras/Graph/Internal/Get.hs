{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Database.Bolt.Extras.Graph.Internal.Get
  (
  -- * Types for requesting nodes and relationships
    NodeGetter (..)
  , RelGetter (..)
  , GetterLike(..)
  , (#)
  , defaultNode
  , defaultRel
  , defaultNodeReturn
  , defaultNodeNotReturn
  , defaultRelReturn
  , defaultRelNotReturn
  , requestGetters
  , allProps
  -- * Types for extracting nodes and relationships
  , NodeResult (..)
  , RelResult (..)
  , relationName
  -- * Graph types
  , GraphGetRequest
  , GraphGetResponse
  -- * Helpers to extract entities from result graph
  , extractNode
  , extractRelation
  , extractNodeId
  , extractRelationId
  , extractNodeAeson
  , extractRelationAeson
  ) where

import           Control.Lens                                      (at, non, to,
                                                                    (^.))
import           Control.Monad.IO.Class                            (MonadIO)
import           Data.Aeson                                        as A (FromJSON (..),
                                                                         Result (..),
                                                                         ToJSON (..),
                                                                         Value,
                                                                         fromJSON,
                                                                         genericParseJSON,
                                                                         genericToJSON,
                                                                         omitNothingFields,
                                                                         toJSON)
import           Data.Aeson.Casing                                 (aesonPrefix,
                                                                    snakeCase)
import           Data.Function                                     ((&))
import           Data.Map.Strict                                   as M (Map,
                                                                         filter,
                                                                         (!))
import           Data.Maybe                                        (catMaybes,
                                                                    fromJust,
                                                                    isJust)
import           Data.Monoid                                       ((<>))
import           Data.Text                                         (Text, cons,
                                                                    intercalate,
                                                                    pack,
                                                                    unpack)
import           Database.Bolt                                     as B (BoltActionT,
                                                                         Node (..),
                                                                         Record,
                                                                         URelationship (..))
import           Database.Bolt.Extras                              (BoltId, GetBoltId (..),
                                                                    Label,
                                                                    NodeLike (..),
                                                                    URelationLike (..))
import           Database.Bolt.Extras.Graph.Internal.AbstractGraph (Graph,
                                                                    relations,
                                                                    vertices)
import           Database.Bolt.Extras.Graph.Internal.Class         (Extractable (..),
                                                                    Returnable (..))
import           Database.Bolt.Extras.Selector                     (GetterLike (..),
                                                                    NodeGetter (..),
                                                                    NodeName,
                                                                    RelGetter (..),
                                                                    Requestable (..),
                                                                    defaultNode,
                                                                    defaultNodeNotReturn,
                                                                    defaultNodeReturn,
                                                                    defaultRel,
                                                                    defaultRelNotReturn,
                                                                    defaultRelReturn,
                                                                    relationName)
import           GHC.Generics                                      (Generic)
import           NeatInterpolation                                 (text)
import           Text.Printf                                       (printf)

----------------------------------------------------------
-- REQUEST --
----------------------------------------------------------


-- | A synonym for '&'. Kept for historical reasons.
(#) :: a -> (a -> b) -> b
(#) = (&)

instance Returnable (NodeName, NodeGetter) where
  isReturned' (_, ng) = ngIsReturned ng

  return' (name, ng)  = let showProps = showRetProps name $ ngReturnProps ng
                        in [text|{ id: id($name),
                                   labels: labels($name),
                                   props: $showProps
                                 } as $name
                           |]

instance Returnable ((NodeName, NodeName), RelGetter) where
  isReturned' (_, rg)            = rgIsReturned rg

  return' ((stName, enName), rg) = let name      = relationName (stName, enName)
                                       showProps = showRetProps name $ rgReturnProps rg
                                   in [text|{ id: id($name),
                                              label: type($name),
                                              props: $showProps
                                            } as $name
                                      |]

-- | Return all properties of a node or relation. To be used with 'withReturn'.
allProps :: [Text]
allProps = ["*"]

showRetProps :: Text -> [Text] -> Text
showRetProps name []    = name <> "{}"
showRetProps name ["*"] = "properties(" <> name <> ")"
showRetProps name props = name <> "{" <> intercalate ", " (cons '.' <$> props) <> "}"

-- | Takes all node getters and relationship getters
-- and write them to single query to request.
-- Also return conditions on known boltId-s.
--
requestGetters :: [(NodeName, NodeGetter)]
               -> [((NodeName, NodeName), RelGetter)]
               -> (Text, [Text])
requestGetters ngs rgs = ("MATCH " <> intercalate ", " (fmap request rgs ++ fmap request ngs), conditionsID)
  where
    boltIdCondN :: (NodeName, NodeGetter) -> Maybe Text
    boltIdCondN (name, ng) = pack . printf "ID(%s)=%d" name <$> ngboltId ng

    boltIdCondR :: ((NodeName, NodeName), RelGetter) -> Maybe Text
    boltIdCondR (names, rg) = pack . printf "ID(%s)=%d" (relationName names) <$> rgboltId rg

    conditionsID  = catMaybes (fmap boltIdCondN ngs ++ fmap boltIdCondR rgs)

----------------------------------------------------------
-- RESULT --
----------------------------------------------------------

-- | Result for node where properties are represented as @aeson@ 'A.Value'.
--
data NodeResult = NodeResult { nresId     :: BoltId
                             , nresLabels :: [Label]
                             , nresProps  :: Map Text A.Value
                             }
  deriving (Show, Eq, Generic)

-- | Result for relation where properties are represented as @aeson@ 'A.Value'.
--
data RelResult = RelResult { rresId    :: BoltId
                           , rresLabel :: Label
                           , rresProps :: Map Text A.Value
                           }
  deriving (Show, Eq, Generic)

instance GetBoltId NodeResult where
  getBoltId = nresId

instance GetBoltId RelResult where
  getBoltId = rresId

instance ToJSON NodeResult where
  toJSON = genericToJSON (aesonPrefix snakeCase)
    { omitNothingFields = True }
instance FromJSON NodeResult where
  parseJSON = genericParseJSON (aesonPrefix snakeCase)
    { omitNothingFields = True }

instance ToJSON RelResult where
  toJSON = genericToJSON (aesonPrefix snakeCase)
    { omitNothingFields = True }
instance FromJSON RelResult where
  parseJSON = genericParseJSON (aesonPrefix snakeCase)
    { omitNothingFields = True }

instance Extractable NodeResult where
  extract = extractFromJSON

instance Extractable RelResult where
  extract = extractFromJSON

extractFromJSON :: (MonadIO m, FromJSON a) => Text -> [Record] -> BoltActionT m [a]
extractFromJSON var = pure . fmap (\r -> case fromJSON (toJSON (r ! var)) of
                                        Success parsed -> parsed
                                        Error   err    -> error err)

fromJSONM :: forall a. FromJSON a => A.Value -> Maybe a
fromJSONM (fromJSON -> Success r :: Result a) = Just r
fromJSONM _                                   = Nothing

instance NodeLike NodeResult where
  toNode NodeResult{..} = Node       nresId       nresLabels (fromJust <$> M.filter isJust (fromJSONM <$> nresProps))
  fromNode Node{..}     = NodeResult nodeIdentity labels     (toJSON   <$> nodeProps)

instance URelationLike RelResult where
  toURelation RelResult{..}       = URelationship rresId       rresLabel (fromJust <$> M.filter isJust (fromJSONM <$> rresProps))
  fromURelation URelationship{..} = RelResult     urelIdentity urelType  (toJSON   <$> urelProps)

----------------------------------------------------------
-- GRAPH --
----------------------------------------------------------

-- | The combinations of getters to load graph from the database.
--
type GraphGetRequest = Graph NodeName NodeGetter RelGetter

-- | The graph of 'Node's and 'URelationship's which we got from the database using 'GraphGetRequest'.
--
type GraphGetResponse = Graph NodeName NodeResult RelResult


-- | Extract a node by its name from 'GraphGetResponse' and convert it to user type
-- with 'fromNode'.
extractNode :: NodeLike a => NodeName -> GraphGetResponse -> a
extractNode var graph = graph ^. vertices . at var . non (errorForNode var) . to (fromNode . toNode)

-- | Extract a relation by name of it start and end nodes and convert to user type with 'fromURelation'.
extractRelation :: URelationLike a => NodeName -> NodeName -> GraphGetResponse -> a
extractRelation stVar enVar graph = graph ^. relations . at (stVar, enVar)
                                  . non (errorForRelation stVar enVar)
                                  . to (fromURelation . toURelation)

-- | Extract just node's 'BoltId'.
extractNodeId :: NodeName -> GraphGetResponse -> BoltId
extractNodeId var graph = graph ^. vertices . at var . non (errorForNode var) . to nresId

-- | Extract just relation's 'BoltId'.
extractRelationId :: NodeName -> NodeName -> GraphGetResponse -> BoltId
extractRelationId stVar enVar graph = graph ^. relations . at (stVar, enVar)
                                    . non (errorForRelation stVar enVar)
                                    . to rresId

-- | Extract 'NodeResult'.
extractNodeAeson :: NodeName -> GraphGetResponse -> NodeResult
extractNodeAeson var graph = graph ^. vertices . at var . non (errorForNode var)

-- | Extract 'RelResult'.
extractRelationAeson :: NodeName -> NodeName -> GraphGetResponse -> RelResult
extractRelationAeson stVar enVar graph = graph ^. relations . at (stVar, enVar)
                                       . non (errorForRelation stVar enVar)

errorForNode :: NodeName -> a
errorForNode name = error . unpack $ "node with name " <> name <> " doesn't exist"

errorForRelation :: NodeName -> NodeName -> a
errorForRelation stName enName = error . unpack $ "relation between nodes " <>
                                                  stName <> " and " <> enName <>
                                                  " doesn't exist"
