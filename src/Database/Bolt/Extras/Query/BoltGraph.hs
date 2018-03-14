{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Database.Bolt.Extras.Query.BoltGraph
    ( Graph (..)
    , QueryGraph
    , ResponseGraph
    , getGraph
    , createGraph
    , vertices
    , relations
    , NodeName
    ) where

import           Control.Applicative                  (liftA2)
import           Control.Lens                         (makeLenses, over)
import           Control.Monad.IO.Class               (MonadIO)
import           Data.Map.Strict                      (toList, (!), insert, mapWithKey, keys)
import qualified Data.Map.Strict                      as M (Map, map)
import qualified Data.Text                            as T (Text, concat,
                                                            intercalate, pack)
import           Database.Bolt                        (BoltActionT, Node (..),
                                                       Record, RecordValue (..),
                                                       Relationship (..),
                                                       URelationship (..),
                                                       exact, query)
import           Database.Bolt.Extras.Query.Cypher    (ToCypher (..))
import           Database.Bolt.Extras.Query.Entity    (EntityLike (..),
                                                       generateEntityVars)
import           Database.Bolt.Extras.Query.Queries   (UploadTypedNode (..),
                                                       createRelationship,
                                                       uploadNode)
import           Database.Bolt.Extras.Query.Selectors (NodeSelector (..),
                                                       URelSelector (..))
import           Database.Bolt.Id                     (BoltId (..))
import           Debug.Trace
import           NeatInterpolation                    (text)
import           Text.Printf                          (printf)


data Graph n a b = Graph { _vertices  :: M.Map n a
                         , _relations :: M.Map (n, n) b
                         } deriving (Show)

makeLenses ''Graph

addNode :: Ord n => n -> a -> Graph n a b -> Graph n a b
addNode name node = over vertices (insert name node)

addRelation :: Ord n => n -> n -> b -> Graph n a b -> Graph n a b
addRelation startName endName rel = over relations (insert (startName, endName) rel)

type NodeName = T.Text

type QueryGraph = Graph NodeName UploadTypedNode URelationship

type ReturnedGraph = Graph NodeName Node URelationship

type ResponseGraph = Graph NodeName BoltId BoltId

-- | The combinations of selectors to load graph from the database.
type QueryGraphSelector = Graph NodeName NodeSelector URelSelector

-- | For the given QueryGraphSelector find the graph, which matches it.
-- This function creates single cypher query and performs it.
getGraph :: (MonadIO m) => QueryGraphSelector -> BoltActionT m ReturnedGraph
getGraph queryGraph = do
  res <- query getQ
  let nodes = sequenceA $ mapWithKey (\key _ -> fmap head (exactValues key res)) vertices
  let edges = sequenceA $ mapWithKey (\key _ -> fmap (makeU . head) (exactValues
                                       (T.concat [fst key, "0", snd key]) res)) rels
  liftA2 Graph nodes edges
  where

    vertices :: M.Map NodeName NodeSelector
    vertices = _vertices queryGraph

    rels :: M.Map (NodeName, NodeName) URelSelector
    rels = _relations queryGraph

    nodeVars :: [T.Text]
    nodeVars = keys vertices
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
      let nodes = T.intercalate "," $ map (\(k, v) -> do
                                            let labels = maybe "" toCypher (labelsS v)
                                            [text|($k $labels)|]) (toList vertices)
      let returnNodes = T.intercalate "," nodeVars
      let conditions = (T.intercalate " AND " . filter (/= "\n")) $ map (\(k, v) -> do
                                    let name = k
                                    let boltIdQR = maybe ""
                                                   (T.pack . printf "ID(%s)=%d" name . boltId)
                                                   (boltIdQ v)
                                    [text|$boltIdQR|]) (toList vertices)
      let edges = T.intercalate "," $ map (\(k, v) -> do
                                            let name   = T.concat [fst k, "0", snd k]
                                            --let rs     = listSelector !! i
                                            let typeQ  = maybe "" toCypher (typeLS v)
                                            let stNode = fst k
                                            let stNodeLabels = maybe "" toCypher $ labelsS (vertices ! stNode)
                                            let endNode = snd k
                                            let endNodeLabels = maybe "" toCypher $ labelsS (vertices ! endNode)
                                            [text|($stNode $stNodeLabels)-[$name $typeQ]-($endNode $endNodeLabels)|])
                                          (toList rels)

      let returnEdges = T.intercalate "," edgesVars

      [text|MATCH $nodes
            WHERE $conditions
            MATCH $edges
            RETURN $returnNodes, $returnEdges|]



    exactValues :: (MonadIO m, RecordValue a) => T.Text -> [Record] -> BoltActionT m [a]
    exactValues var = mapM (exact . (! var))

-- | Create Graph using given GraphU and the list describing 'Node's indices (from the given _vertices),
-- which should be connected by the corresponding 'Relationship'.
-- If there were multiple choices while merging given _vertices, the first match is used for connection.
createGraph :: (MonadIO m) => QueryGraph -> BoltActionT m ResponseGraph
createGraph queryGraph = do
  let vertices = _vertices queryGraph
  let rels = _relations queryGraph
  nodes <- sequenceA $ M.map (fmap head . uploadNode) vertices
  edges <- sequenceA $
          mapWithKey (\key v -> do
              let stNode  = nodes ! fst key
              let endNode = nodes ! snd key
              createRelationship stNode v endNode) rels
  return $ Graph nodes edges


