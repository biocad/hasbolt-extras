{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Database.Bolt.Extras.Query.BoltGraph
    ( Graph (..)
    , NodeName
    , QueryGraph
    , ResponseGraph
    , addNode
    , addRelation
    , createGraph
    , emptyGraph
    , getGraph
    , relations
    , vertices
    ) where

import           Control.Applicative                  (liftA2)
import           Control.Lens                         (makeLenses, over)
import           Control.Monad.IO.Class               (MonadIO)
import           Data.Map.Strict                      (Map, insert, keys,
                                                       mapWithKey, notMember,
                                                       toList, (!))
import qualified Data.Map.Strict                      as M (map)
import qualified Data.Text                            as T (Text, concat, empty,
                                                            intercalate, pack,
                                                            unpack)
import           Database.Bolt                        (BoltActionT, Node (..),
                                                       Record, RecordValue (..),
                                                       Relationship (..),
                                                       URelationship (..),
                                                       exact, query)
import           Database.Bolt.Extras.Query.Cypher    (ToCypher (..))
import           Database.Bolt.Extras.Query.Queries   (UploadTypedNode (..),
                                                       createRelationship,
                                                       uploadNode)
import           Database.Bolt.Extras.Query.Selectors (NodeSelector (..),
                                                       URelSelector (..))
import           Database.Bolt.Id                     (BoltId (..))
import           Debug.Trace
import           NeatInterpolation                    (text)
import           Text.Printf                          (printf)


data Graph n a b = Graph { _vertices  :: Map n a
                         , _relations :: Map (n, n) b
                         } deriving (Show)

makeLenses ''Graph

emptyGraph :: Ord n => Graph n a b
emptyGraph = Graph mempty mempty

addNode :: Ord n => n -> a -> Graph n a b -> Graph n a b
addNode name node graph = if name `notMember` (_vertices graph)
                          then over vertices (insert name node) graph
                          else error "vertex' key already exists"

addRelation :: Ord n => n -> n -> b -> Graph n a b -> Graph n a b
addRelation startName endName rel graph = if (startName, endName) `notMember` (_relations graph)
                                          then over relations (insert (startName, endName) rel) graph
                                          else error "relation' key already exists"

type NodeName = T.Text

type QueryGraph = Graph NodeName UploadTypedNode URelationship

type ReturnedGraph = Graph NodeName Node URelationship

type ResponseGraph = Graph NodeName BoltId BoltId

-- | The combinations of selectors to load graph from the database.
type QueryGraphSelector = Graph NodeName NodeSelector URelSelector

-- | For the given QueryGraphSelector find the graph, which matches it.
-- This function creates single cypher query and performs it.
getGraph :: (MonadIO m) => [T.Text] -> QueryGraphSelector -> BoltActionT m [ReturnedGraph]
getGraph customConds queryGraph = do
  res <- trace (T.unpack getQ) $ query getQ
  --let temp = exactValues nodeVars res
  --let nodesL = fmap (\node -> sequenceA $ mapWithKey (\key _ -> node)) (exactValues nodeVars res)
  traverse (\i -> do
            let nodes = sequenceA $ mapWithKey (\key _ -> fmap (!! i) (exactValues key res)) vertices'
            let edges = sequenceA $ mapWithKey (\key _ -> fmap (makeU . (!! i)) (exactValues
                                       (T.concat [fst key, "0", snd key]) res)) rels
            liftA2 Graph nodes edges) [0..(length res) - 1]
  --let edgesL = fmap (\rel -> sequenceA $ mapWithKey (\key _ -> rel)) (exactValues
  --                                     edgesVars res)
  --trace (show nodesL) $ return []
  where

    vertices' :: Map NodeName NodeSelector
    vertices' = _vertices queryGraph

    rels :: Map (NodeName, NodeName) URelSelector
    rels = _relations queryGraph

    nodeVars :: [T.Text]
    nodeVars = keys vertices'
    --nodeVars = map toCypher $ generateEntityVars (map toEntity
    --              [ (Node i lbl (fromList [])) | vs <- verticesSelector,
    --                                             let i = (boltId . fromMaybe (fromInt (-1)) . boltIdQ) vs,
    --                                             let lbl = (fromMaybe [] . labelsS) vs])

    makeU :: Relationship -> URelationship
    makeU Relationship{..} = URelationship relIdentity relType relProps

    edgesVars :: [T.Text]
    edgesVars = map (\k -> T.concat [fst k, "0", snd k]) (keys rels)

    getQ :: T.Text
    getQ = do
      let nodes = map (\(k, v) -> do
                                            let labels = maybe "" toCypher (labelsS v)
                                            let propsQ = maybe "" (\props -> T.concat ["{", toCypher props, "}"]) (propsS v)
                                            [text|($k $labels $propsQ)|]) (toList vertices')
      let returnNodes = nodeVars
      let conditionsId = T.intercalate " AND " . filter (/= "\n") $ map (\(k, v) -> do
                                    let name = k
                                    let boltIdQR = maybe ""
                                                   (T.pack . printf "ID(%s)=%d" name . boltId)
                                                   (boltIdQ v)
                                    [text|$boltIdQR|]) (toList vertices')
      let customConditions = T.intercalate " AND " customConds
      let conditions = T.intercalate " AND " . filter (/= T.empty) $ [conditionsId, customConditions]
      let conditionsQ = if conditions == T.empty then "" else T.concat ["WHERE ", conditions]
      let edges = map (\(k, v) -> do
                                            let name   = T.concat [fst k, "0", snd k]
                                            --let rs     = listSelector !! i
                                            let typeQ  = maybe "" toCypher (typeLS v)
                                            let stNodeName = fst k
                                            let stNode = vertices' ! stNodeName
                                            let stNodeLabels = maybe "" toCypher $ labelsS stNode
                                            let stNodeProps = maybe "" (\props -> T.concat ["{", toCypher props, "}"]) (propsS stNode)
                                            let endNodeName = snd k
                                            let endNode = vertices' ! endNodeName
                                            let endNodeLabels = maybe "" toCypher $ labelsS endNode
                                            let endNodeProps = maybe "" (\props -> T.concat ["{", toCypher props, "}"]) (propsS endNode)
                                            let propsQ = maybe "" (\props -> T.concat ["{", toCypher props, "}"]) (propsLS v)
                                            [text|($stNodeName $stNodeLabels $stNodeProps)-[$name $typeQ $propsQ]-($endNodeName $endNodeLabels $endNodeProps)|])
                                          (toList rels)

      let returnEdges = edgesVars
      let completeRequest = T.intercalate "," $ nodes ++ edges
      let completeResponse = T.intercalate "," $ returnNodes ++ returnEdges

      [text|MATCH $completeRequest
            $conditionsQ
            RETURN $completeResponse|]



    exactValues :: (MonadIO m, RecordValue a) => T.Text -> [Record] -> BoltActionT m [a]
    exactValues var = mapM (exact . (! var))
    --exactValues :: (MonadIO m, RecordValue a) => [T.Text] -> [Record] -> BoltActionT m [[a]]
    --exactValues vars recs = mapM (\var -> mapM (exact . (! var)) recs) vars

-- | Create Graph using given GraphU and the list describing 'Node's indices (from the given _vertices),
-- which should be connected by the corresponding 'Relationship'.
-- If there were multiple choices while merging given _vertices, the first match is used for connection.
createGraph :: (MonadIO m) => QueryGraph -> BoltActionT m ResponseGraph
createGraph queryGraph = do
  let vertices' = _vertices queryGraph
  let rels = _relations queryGraph
  nodes <- sequenceA $ M.map (fmap head . uploadNode) vertices'
  edges <- sequenceA $
          mapWithKey (\key v -> do
              let stNode  = nodes ! fst key
              let endNode = nodes ! snd key
              createRelationship stNode v endNode) rels
  return $ Graph nodes edges


