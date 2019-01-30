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
  , GetterLike (..)
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
                                                                         fromList,
                                                                         insert,
                                                                         toList,
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
                                                                         URelationship (..),
                                                                         Value)
import           Database.Bolt.Extras                              (BoltId, GetBoltId (..),
                                                                    Label,
                                                                    NodeLike (..),
                                                                    ToCypher (..),
                                                                    URelationLike (..))
import           Database.Bolt.Extras.Graph.Internal.AbstractGraph (Graph,
                                                                    NodeName,
                                                                    relationName,
                                                                    relations,
                                                                    vertices)
import           Database.Bolt.Extras.Graph.Internal.Class         (Extractable (..),
                                                                    Requestable (..),
                                                                    Returnable (..))
import           GHC.Generics                                      (Generic)
import           Language.Haskell.TH.Syntax                        (Name,
                                                                    nameBase)
import           NeatInterpolation                                 (text)
import           Text.Printf                                       (printf)

----------------------------------------------------------
-- REQUEST --
----------------------------------------------------------

-- | Helper to find 'Node's.
--
data NodeGetter = NodeGetter { ngboltId      :: Maybe BoltId     -- ^ known boltId
                             , ngLabels      :: [Label]          -- ^ known labels
                             , ngProps       :: Map Text B.Value -- ^ known properties
                             , ngReturnProps :: [Text]           -- ^ names of properties to return
                             , ngIsReturned  :: Bool             -- ^ whether return this node or not
                             }
  deriving (Show, Eq)

-- | Helper to find 'URelationship's.
--
data RelGetter = RelGetter { rgboltId      :: Maybe BoltId     -- ^ known boltId
                           , rgLabel       :: Maybe Label      -- ^ known labels
                           , rgProps       :: Map Text B.Value -- ^ known properties
                           , rgReturnProps :: [Text]           -- ^ names of properties to return
                           , rgIsReturned  :: Bool             -- ^ whether return this relation or not
                           }
  deriving (Show, Eq)

(#) :: a -> (a -> b) -> b
(#) = (&)

defaultNode :: Bool -> NodeGetter
defaultNode = NodeGetter Nothing [] (fromList []) []

defaultRel :: Bool -> RelGetter
defaultRel = RelGetter Nothing Nothing (fromList []) []

defaultNodeReturn :: NodeGetter
defaultNodeReturn = defaultNode True

defaultNodeNotReturn :: NodeGetter
defaultNodeNotReturn = defaultNode False

defaultRelReturn :: RelGetter
defaultRelReturn = defaultRel True

defaultRelNotReturn :: RelGetter
defaultRelNotReturn = defaultRel False

-- | Helper to work with Getters.
--
class GetterLike a where
    withBoltId :: BoltId          -> a -> a -- ^ set known boltId
    withLabel  :: Label           -> a -> a -- ^ set known label
    withLabelQ :: Name            -> a -> a -- ^ set known label as 'Name'
    withProp   :: (Text, B.Value) -> a -> a -- ^ add known property
    withReturn :: [Text]          -> a -> a -- ^ add list of properties to return
    isReturned ::                    a -> a -- ^ set that current node should be returned

instance GetterLike NodeGetter where
    withBoltId boltId ng = ng { ngboltId       = Just boltId }
    withLabel  lbl    ng = ng { ngLabels       = lbl : ngLabels ng }
    withLabelQ lblQ      = withLabel (pack . nameBase $ lblQ)
    withProp (pk, pv) ng = ng { ngProps        = insert pk pv (ngProps ng) }
    withReturn props  ng = ng { ngReturnProps  = ngReturnProps ng ++ props }
    isReturned        ng = ng { ngIsReturned   = True }

instance GetterLike RelGetter where
    withBoltId boltId rg = rg { rgboltId       = Just boltId }
    withLabel  lbl    rg = rg { rgLabel        = Just lbl    }
    withLabelQ lblQ      = withLabel (pack . nameBase $ lblQ)
    withProp (pk, pv) rg = rg { rgProps        = insert pk pv (rgProps rg) }
    withReturn props  rg = rg { rgReturnProps  = rgReturnProps rg ++ props }
    isReturned        rg = rg { rgIsReturned   = True }

instance Requestable (NodeName, NodeGetter) where
  request (name, ng) = [text|($name $labels $propsQ)|]
    where
      labels = toCypher . ngLabels $ ng
      propsQ = "{" <> (toCypher . toList . ngProps $ ng) <> "}"

instance Requestable ((NodeName, NodeName), RelGetter) where
  request ((stName, enName), rg) = [text|($stName)-[$name $typeQ $propsQ]-($enName)|]
    where
      name   = relationName (stName, enName)
      typeQ  = maybe "" toCypher (rgLabel rg)
      propsQ = "{" <> (toCypher . toList . rgProps $ rg) <> "}"

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

-- | Result for node in the Aeson like format.
--
data NodeResult = NodeResult { nresId     :: BoltId
                             , nresLabels :: [Label]
                             , nresProps  :: Map Text A.Value
                             }
  deriving (Show, Eq, Generic)

-- | Result for relationship in the Aeson like format.
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

-- | The combinations of 'Getter's to load graph from the database.
--
type GraphGetRequest = Graph NodeName NodeGetter RelGetter

-- | The graph of 'Node's and 'URelationship's which we got from the database using 'GraphGetRequest'.
--
type GraphGetResponse = Graph NodeName NodeResult RelResult

-- | Some helpers to extract entities from the result graph.

extractNode :: NodeLike a => NodeName -> GraphGetResponse -> a
extractNode var graph = graph ^. vertices . at var . non (errorForNode var) . to (fromNode . toNode)

extractRelation :: URelationLike a => NodeName -> NodeName -> GraphGetResponse -> a
extractRelation stVar enVar graph = graph ^. relations . at (stVar, enVar)
                                  . non (errorForRelation stVar enVar)
                                  . to (fromURelation . toURelation)

extractNodeId :: NodeName -> GraphGetResponse -> BoltId
extractNodeId var graph = graph ^. vertices . at var . non (errorForNode var) . to nresId

extractRelationId :: NodeName -> NodeName -> GraphGetResponse -> BoltId
extractRelationId stVar enVar graph = graph ^. relations . at (stVar, enVar)
                                    . non (errorForRelation stVar enVar)
                                    . to rresId

extractNodeAeson :: NodeName -> GraphGetResponse -> NodeResult
extractNodeAeson var graph = graph ^. vertices . at var . non (errorForNode var)

extractRelationAeson :: NodeName -> NodeName -> GraphGetResponse -> RelResult
extractRelationAeson stVar enVar graph = graph ^. relations . at (stVar, enVar)
                                       . non (errorForRelation stVar enVar)

errorForNode :: NodeName -> a
errorForNode name = error . unpack $ "node with name " <> name <> " doesn't exist"

errorForRelation :: NodeName -> NodeName -> a
errorForRelation stName enName = error . unpack $ "relation between nodes " <>
                                                  stName <> " and " <> enName <>
                                                  " doesn't exist"
