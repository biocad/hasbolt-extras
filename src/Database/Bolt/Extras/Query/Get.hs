{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Database.Bolt.Extras.Query.Get
    ( NodeGetter (..)
    , GraphGetRequest
    , GraphGetResponse
    , RelGetter (..)
    , getGraph
    ) where

import           Control.Monad.IO.Class              (MonadIO)
import           Data.Map.Strict                     (Map, keys, mapWithKey,
                                                      toList, (!))
import qualified Data.Text                           as T (Text, concat, empty,
                                                           intercalate, pack)
import           Database.Bolt                       (BoltActionT, Node (..),
                                                      Record, RecordValue (..),
                                                      Relationship (..),
                                                      URelationship (..), exact,
                                                      query)
import           Database.Bolt.Extras.Graph          (Graph (..))
import           Database.Bolt.Extras.Persisted      (BoltId)
import           Database.Bolt.Extras.Query.Cypher   (ToCypher (..))
import           Database.Bolt.Extras.Query.Utils    (NodeName)
import           Database.Bolt.Extras.Template.Types (Label, Property)
import           NeatInterpolation                   (text)
import           Text.Printf                         (printf)

-- | Helper to find 'Node's.
-- _varQNName is the mark for this Node, which will be used in Cypher queries.
-- For example "MATCH(a)", here _varQNName = "a"
data NodeGetter = NodeGetter { boltIdN :: Maybe BoltId
                             , labelsN :: Maybe [Label]
                             , propsN  :: Maybe [Property]
                             } deriving (Show)

-- | RelGetter is used for searching using indexes of 'Node's in the given graph.
data RelGetter = RelGetter { labelR :: Maybe Label
                           , propsR :: Maybe [Property]
                           } deriving (Show)

-- | The combinations of Getters to load graph from the database.
type GraphGetRequest = Graph NodeName NodeGetter RelGetter

type GraphGetResponse = Graph NodeName Node URelationship

-- | For the given GraphGetRequest find the graph, which matches it.
-- This function creates single cypher query and performs it.
getGraph :: (MonadIO m) => [T.Text] -> GraphGetRequest -> BoltActionT m [GraphGetResponse]
getGraph customConds requestGraph = do
  response <- query (formQuery customConds nodeVars edgesVars vertices rels)
  mapM (\i -> do
      nodes <- sequence $ mapOnlyKey (fmap (!! i) . flip exactValues response) vertices
      edges <- sequence $ mapOnlyKey (fmap (makeU . (!! i)) . flip exactValues response . namesToText) rels
      return (Graph nodes edges)) [0.. length response - 1]
  where
    vertices :: Map NodeName NodeGetter
    vertices = _vertices requestGraph

    rels :: Map (NodeName, NodeName) RelGetter
    rels = _relations requestGraph

    nodeVars :: [T.Text]
    nodeVars = keys vertices

    edgesVars :: [T.Text]
    edgesVars = map (\k -> T.concat [fst k, "0", snd k]) (keys rels)

    exactValues :: (MonadIO m, RecordValue a) => T.Text -> [Record] -> BoltActionT m [a]
    exactValues var = mapM (exact . (! var))

    makeU :: Relationship -> URelationship
    makeU Relationship{..} = URelationship relIdentity relType relProps

    namesToText :: (NodeName, NodeName) -> T.Text
    namesToText (nameA, nameB) = T.concat [nameA, "0", nameB]

    mapOnlyKey :: (k -> b) -> Map k a -> Map k b
    mapOnlyKey f = mapWithKey (\k _ -> f k)

formQuery :: [T.Text] -> [T.Text] -> [T.Text] -> Map NodeName NodeGetter -> Map (NodeName, NodeName) RelGetter -> T.Text
formQuery customConds returnNodes returnEdges vertices rels =
  [text|MATCH $completeRequest
        $conditionsQ
        RETURN $completeResponse|]
  where
    nodes = nodeAsText <$> toList vertices

    conditionsId     = intercalateAnd . filter (/= "\n") $ fmap condIdAsText (toList vertices)
    customConditions = intercalateAnd customConds
    conditions       = intercalateAnd . filter (/= T.empty) $ [conditionsId, customConditions]
    conditionsQ      = if conditions == T.empty then "" else T.concat ["WHERE ", conditions]

    edges = fmap (relationshipAsText vertices) (toList rels)

    completeRequest  = T.intercalate "," $ nodes ++ edges
    completeResponse = T.intercalate "," $ returnNodes ++ returnEdges

    intercalateAnd :: [T.Text] -> T.Text
    intercalateAnd = T.intercalate " AND "

condIdAsText :: (NodeName, NodeGetter) -> T.Text
condIdAsText (name, sel) = [text|$boltIdNR|]
  where
    boltIdNR = maybeNull (T.pack . printf "ID(%s)=%d" name) (boltIdN sel)

nodeAsText :: (NodeName, NodeGetter) -> T.Text
nodeAsText (name, sel) = [text|($name $labels $propsQ)|]
  where
    labels = maybeNull toCypher (labelsN sel)
    propsQ = maybeNull (\props -> T.concat ["{", toCypher props, "}"]) (propsN sel)

relationshipAsText :: Map NodeName NodeGetter -> ((NodeName, NodeName), RelGetter) -> T.Text
relationshipAsText vertices ((begNodeName, endNodeName), uRel) =
  [text|($begNodeName $begNodeLabels $begNodeProps)-[$name $typeQ $propsQ]-($endNodeName $endNodeLabels $endNodeProps)|]
  where
    name   = T.concat [begNodeName, "0", endNodeName]
    typeQ  = maybeNull toCypher (labelR uRel)

    begNode       = vertices ! begNodeName
    begNodeLabels = maybeNull toCypher $ labelsN begNode
    begNodeProps  = maybeNull (\props -> T.concat ["{", toCypher props, "}"]) (propsN begNode)

    endNode       = vertices ! endNodeName
    endNodeLabels = maybeNull toCypher $ labelsN endNode
    endNodeProps  = maybeNull (\props -> T.concat ["{", toCypher props, "}"]) (propsN endNode)

    propsQ = maybeNull (\props -> T.concat ["{", toCypher props, "}"]) (propsR uRel)

maybeNull :: (a -> T.Text) -> Maybe a -> T.Text
maybeNull = maybe ""
