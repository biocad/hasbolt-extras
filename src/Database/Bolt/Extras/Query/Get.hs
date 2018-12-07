{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

module Database.Bolt.Extras.Query.Get
    ( NodeGetter (..)
    , GraphGetRequest
    , GraphGetResponse
    , RelGetter (..)
    , getGraph
    , nodeAsText
    , condIdAsText
    , mergeGraphs
    ) where

import           Control.Lens                        (over, (^.))
import           Control.Monad.IO.Class              (MonadIO)
import           Data.List                           (foldl')
import           Data.Map.Strict                     (Map, keys, mapKeys,
                                                      mapWithKey, toList, union,
                                                      (!))
import           Data.Monoid                         ((<>))
import qualified Data.Text                           as T (Text, concat, empty,
                                                           intercalate, pack)
import           Database.Bolt                       (BoltActionT, Node (..),
                                                      Record, RecordValue (..),
                                                      Relationship (..),
                                                      URelationship (..), exact,
                                                      query)
import           Database.Bolt.Extras.Graph          (Graph (..), emptyGraph,
                                                      relations, vertices)
import           Database.Bolt.Extras.Persisted      (BoltId)
import           Database.Bolt.Extras.Query.Cypher   (ToCypher (..))
import           Database.Bolt.Extras.Query.Utils    (NodeName)
import           Database.Bolt.Extras.Template.Types (Label, Property)
import           NeatInterpolation                   (text)
import           Text.Printf                         (printf)

-- | Helper to find 'Node's.
--
data NodeGetter = NodeGetter { boltIdN :: Maybe BoltId
                             , labelsN :: Maybe [Label]
                             , propsN  :: Maybe [Property]
                             } deriving (Show)

-- | Helper to find 'URelationship's.
--
data RelGetter = RelGetter { labelR :: Maybe Label
                           , propsR :: Maybe [Property]
                           } deriving (Show)

-- | The combinations of 'Getter's to load graph from the database.
--
type GraphGetRequest = Graph NodeName NodeGetter RelGetter

-- | The graph of 'Node's and 'URelationship's which we got from the database using 'GraphGetRequest'.
--
type GraphGetResponse = Graph NodeName Node URelationship

-- | For the given 'GraphGetRequest' find all graphs, which match it.
-- This function creates single cypher query and performs it.
--
getGraph :: (MonadIO m) => [T.Text] -> GraphGetRequest -> BoltActionT m [GraphGetResponse]
getGraph customConds requestGraph = do
  response <- query (formQuery customConds nodeVars edgesVars (requestGraph ^. vertices) (requestGraph ^. relations))
  mapM (\i -> do
      nodes <- sequence $ mapOnlyKey (fmap (!! i) . flip exactValues response) (requestGraph ^. vertices)
      edges <- sequence $ mapOnlyKey (fmap (makeU . (!! i)) . flip exactValues response . namesToText) (requestGraph ^. relations)
      return (Graph nodes edges)) [0.. length response - 1]
  where
    nodeVars :: [T.Text]
    nodeVars = keys $ requestGraph ^. vertices

    edgesVars :: [T.Text]
    edgesVars = map (\k -> T.concat [fst k, "0", snd k]) (keys $ requestGraph ^. relations)

    exactValues :: (MonadIO m, RecordValue a) => T.Text -> [Record] -> BoltActionT m [a]
    exactValues var = mapM (exact . (! var))

    makeU :: Relationship -> URelationship
    makeU Relationship{..} = URelationship relIdentity relType relProps

    namesToText :: (NodeName, NodeName) -> T.Text
    namesToText (nameA, nameB) = T.concat [nameA, "0", nameB]

    mapOnlyKey :: (k -> b) -> Map k a -> Map k b
    mapOnlyKey f = mapWithKey (\k _ -> f k)


-- | This function creates cypher query, which is used for getting graph from the database.
--
formQuery :: [T.Text] -> [T.Text] -> [T.Text] -> Map NodeName NodeGetter -> Map (NodeName, NodeName) RelGetter -> T.Text
formQuery customConds returnNodes returnEdges vertices' rels =
  [text|MATCH $completeRequest
        $conditionsQ
        RETURN $completeResponse|]
  where
    nodes = nodeAsText <$> toList vertices'

    conditionsId     = intercalateAnd . filter (/= "\n") $ fmap condIdAsText (toList vertices')
    customConditions = intercalateAnd customConds
    conditions       = intercalateAnd . filter (/= T.empty) $ [conditionsId, customConditions]
    conditionsQ      = if conditions == T.empty then "" else T.concat ["WHERE ", conditions]

    edges = fmap (relationshipAsText vertices') (toList rels)

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
relationshipAsText vertices' ((begNodeName, endNodeName), uRel) =
  [text|($begNodeName $begNodeLabels $begNodeProps)-[$name $typeQ $propsQ]-($endNodeName $endNodeLabels $endNodeProps)|]
  where
    name   = T.concat [begNodeName, "0", endNodeName]
    typeQ  = maybeNull toCypher (labelR uRel)

    begNode       = vertices' ! begNodeName
    begNodeLabels = maybeNull toCypher $ labelsN begNode
    begNodeProps  = maybeNull (\props -> T.concat ["{", toCypher props, "}"]) (propsN begNode)

    endNode       = vertices' ! endNodeName
    endNodeLabels = maybeNull toCypher $ labelsN endNode
    endNodeProps  = maybeNull (\props -> T.concat ["{", toCypher props, "}"]) (propsN endNode)

    propsQ = maybeNull (\props -> T.concat ["{", toCypher props, "}"]) (propsR uRel)

maybeNull :: (a -> T.Text) -> Maybe a -> T.Text
maybeNull = maybe ""

mergeGraphs :: [GraphGetResponse] -> GraphGetResponse
mergeGraphs graphs = foldl' mergeGraph emptyGraph (updateGraph <$> graphs)
  where
    updateGraph :: GraphGetResponse -> GraphGetResponse
    updateGraph graph = do
        let namesMap     = (\name node -> name <> (T.pack . show . nodeIdentity $ node)) `mapWithKey` (graph ^. vertices)
        let newVertices  = mapKeys (\name -> namesMap ! name) (graph ^. vertices)
        let newRelations = mapKeys (\(startName, endName) -> (namesMap ! startName, namesMap ! endName)) (graph ^. relations)
        Graph newVertices newRelations

    mergeGraph :: GraphGetResponse -> GraphGetResponse -> GraphGetResponse
    mergeGraph graphToMerge initialGraph = over relations (union (graphToMerge ^. relations)) $
                                           over vertices  (union (graphToMerge ^. vertices))
                                           initialGraph
