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
  , requestGetters
  -- * Types for extracting nodes and relationships
  , NodeResult (..)
  , RelResult (..)
  , relationName
  -- * Graph types
  , GraphGetRequest
  , GraphGetResponseA
  , GraphGetResponseB
  ) where

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
                                                                    pack)
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
                                                                    relationName)
import           Database.Bolt.Extras.Graph.Internal.Class         (Extractable (..),
                                                                    Requestable (..),
                                                                    Returnable (..))
import           GHC.Generics                                      (Generic)
import           NeatInterpolation                                 (text)
import           Text.Printf                                       (printf)

----------------------------------------------------------
-- REQUEST --
----------------------------------------------------------

-- | Helper to find 'Node's.
--
data NodeGetter = NodeGetter { ngboltId      :: Maybe BoltId
                             , ngLabels      :: [Label]
                             , ngProps       :: Map Text B.Value
                             , ngReturnProps :: [Text]
                             }
  deriving (Show, Eq)

-- | Helper to find 'URelationship's.
--
data RelGetter = RelGetter { rgboltId      :: Maybe BoltId
                           , rgLabel       :: Maybe Label
                           , rgProps       :: Map Text B.Value
                           , rgReturnProps :: [Text]
                           }
  deriving (Show, Eq)

(#) :: a -> (a -> b) -> b
(#) = (&)

defaultNode :: NodeGetter
defaultNode = NodeGetter Nothing [] (fromList []) []

defaultRel :: RelGetter
defaultRel = RelGetter Nothing Nothing (fromList []) []

-- | Helper to work with Getters.
--
class GetterLike a where
  withBoltId :: BoltId          -> a -> a
  withLabel  :: Label           -> a -> a
  withProp   :: (Text, B.Value) -> a -> a
  withReturn :: [Text]          -> a -> a

instance GetterLike NodeGetter where
    withBoltId boltId ng = ng { ngboltId      = Just boltId }
    withLabel  lbl    ng = ng { ngLabels      = lbl : ngLabels ng }
    withProp (pk, pv) ng = ng { ngProps       = insert pk pv (ngProps ng) }
    withReturn props  ng = ng { ngReturnProps = ngReturnProps ng ++ props }

instance GetterLike RelGetter where
    withBoltId boltId rg = rg { rgboltId      = Just boltId }
    withLabel  lbl    rg = rg { rgLabel       = Just lbl    }
    withProp (pk, pv) rg = rg { rgProps       = insert pk pv (rgProps rg) }
    withReturn props  rg = rg { rgReturnProps = rgReturnProps rg ++ props }

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
  return' (name, ng) = let showProps = showRetProps name $ ngReturnProps ng
                          in [text|{ id: id($name),
                                     labels: labels($name),
                                     props: $showProps
                                   } as $name
                              |]

instance Returnable ((NodeName, NodeName), RelGetter) where
  return' ((stName, enName), rg) = let name      = relationName (stName, enName)
                                       showProps = showRetProps name $ rgReturnProps rg
                                      in [text|{ id: id($name),
                                                 label: type($name),
                                                 props: $showProps
                                               } as $name
                                          |]

showRetProps :: Text -> [Text] -> Text
showRetProps name []    = "properties(" <> name <> ")"
showRetProps name props = name <> "{" <> intercalate ", " (cons '.' <$> props) <> "}"

-- | Takes all node getters and relationship getters
-- and write them to single query to request.
--
requestGetters :: [(NodeName, NodeGetter)]
               -> [((NodeName, NodeName), RelGetter)]
               -> Text
requestGetters ngs rgs = "MATCH " <> intercalate ", " (fmap request ngs ++ fmap request rgs) <> conditionsIDQ
  where
    boltIdCondN :: (NodeName, NodeGetter) -> Maybe Text
    boltIdCondN (name, ng) = pack . printf "ID(%s)=%d" name <$> ngboltId ng

    boltIdCondR :: ((NodeName, NodeName), RelGetter) -> Maybe Text
    boltIdCondR (names, rg) = pack . printf "ID(%s)=%d" (relationName names) <$> rgboltId rg

    conditionsID  = catMaybes (fmap boltIdCondN ngs ++ fmap boltIdCondR rgs)
    conditionsIDQ = if null conditionsID then "" else " WHERE " <> intercalate " AND " conditionsID

----------------------------------------------------------
-- RESULT --
----------------------------------------------------------

-- | AESON FORMAT

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

----------------------------------------------------------
-- | BOLT FORMAT

instance Extractable Node where
  extract :: forall m. MonadIO m => Text -> [Record] -> BoltActionT m [Node]
  extract t rec = (toNode <$>) <$> (extractFromJSON t rec :: BoltActionT m [NodeResult])

instance Extractable URelationship where
  extract :: forall m. MonadIO m => Text -> [Record] -> BoltActionT m [URelationship]
  extract t rec = (toURelation <$>) <$> (extractFromJSON t rec :: BoltActionT m [RelResult])

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
-- GRAPH TYPES --
----------------------------------------------------------

-- | The combinations of 'Getter's to load graph from the database.
--
type GraphGetRequest = Graph NodeName NodeGetter RelGetter

-- | The graph of 'Node's and 'URelationship's which we got from the database using 'GraphGetRequest',
-- converted to the Aeson Value like.
--
type GraphGetResponseA = Graph NodeName NodeResult RelResult

-- | The graph of 'Node's and 'URelationship's which we got from the database using 'GraphGetRequest',
-- converted to the Bolt Value like.
--
type GraphGetResponseB = Graph NodeName Node URelationship
