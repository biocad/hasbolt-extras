{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Database.Bolt.Extras.Query.Queries
  (
    mergeNode
  , createRelationship
  , getNodes
  , getRelationships
  , getGraph
  , createGraph
  , NodeSelector (..)
  , RelSelector (..)
  , GraphSelector (..)
  , Graph (..)
  ) where

import           Control.Applicative               (liftA2)
import           Control.Monad                     (forM)
import           Data.List                         (foldl')
import           Data.Map.Strict                   (toList, (!))
import qualified Data.Text                         as T (Text, intercalate,
                                                         pack, unpack)
import           Database.Bolt                     (BoltActionT, Node (..),
                                                    Record, Relationship (..),
                                                    URelationship (..), at,
                                                    exact, query)
import           Database.Bolt.Extras.Query.Cypher (ToCypher (..))
import           Database.Bolt.Extras.Query.Entity (EntityLike (..),
                                                    generateEntityVars)
import           Database.Bolt.Id                  (BoltId (..), GetBoltId (..))
import           NeatInterpolation                 (text)
import           Text.Printf                       (printf)
-- | For given @Node _ labels nodeProps@ makes query @MERGE (n:labels {props}) RETURN ID(n) as n@
-- and then return 'Node' with actual ID.
--
-- Potentially, if you MERGE some 'Node' and it labels and props are occured in
-- several 'Node's, then the result can be not one but several 'Node's.
--
mergeNode :: Node -> BoltActionT IO [Node]
mergeNode node@Node{..} = do
  records      <- query mergeQ
  forM records $ \record -> do
    nodeIdentity' <- record `at` varQ >>= exact
    pure $ node {nodeIdentity = nodeIdentity'}
  where
    [var] = generateEntityVars [toEntity node]
    varQ  = toCypher var

    labelsQ = toCypher labels
    propsQ  = toCypher . toList $ nodeProps

    mergeQ :: T.Text
    mergeQ = [text|MERGE ($varQ $labelsQ {$propsQ})
                   RETURN ID($varQ) as $varQ|]


-- | Return Relationship composed from given URelationship and 'Id's of connected 'Node's
fromU :: Int -> URelationship -> Int -> Relationship
fromU st URelationship{..} end = Relationship urelIdentity st end urelType urelProps


-- | Every relationship in Bolt protocol starts from one 'Node' and ends in anoter.
-- For given starting and ending 'Node's, and for @URelationship  _ urelType urelProps@
-- this method makes MERGE query and then return 'Relationship' with actual ID.
createRelationship :: BoltId -> URelationship -> BoltId -> BoltActionT IO Relationship
createRelationship start urel@URelationship{..} end = do
  [record]      <- query mergeQ
  urelIdentity' <- record `at` varQ >>= exact
  pure $ fromU (boltId start) (urel {urelIdentity = urelIdentity'}) (boltId end)
  where
    [var] = generateEntityVars [toEntity urel]
    varQ = toCypher var

    mergeQ :: T.Text
    mergeQ = do
      let labelQ = toCypher urelType
      let propsQ = toCypher . toList $ urelProps
      let startT = T.pack . show . boltId $ start
      let endT = T.pack . show . boltId $ end

      [text|MATCH (a), (b)
            WHERE ID(a) = $startT AND ID(b) = $endT
            MERGE (a)-[$varQ $labelQ {$propsQ}]->(b)
            RETURN ID($varQ) as $varQ|]


-- | Helper to find 'Node's.
-- _varQNName is the mark for this Node, which will be used in Cypher queries.
-- For example "MATCH(a)", here _varQNName = "a"
data NodeSelector = NodeSelector { _boltIdQ   :: Maybe BoltId
                                 , _varQNName :: T.Text
                                 , _labelsS   :: Maybe [T.Text]
                                 }

-- | Using the given NodeSelector returns the list of all 'Node's, matching it.
getNodes :: NodeSelector -> BoltActionT IO [Node]
getNodes NodeSelector{..} = query getQ >>= exactNodes
  where
    getQ :: T.Text
    getQ = do
      let idQuard = maybe "" (T.pack . printf " WHERE ID(%s)=%d " (T.unpack _varQNName) . boltId) _boltIdQ
      let labelQuard = maybe "" toCypher _labelsS
      [text|MATCH (${_varQNName} $labelQuard) $idQuard
            RETURN ${_varQNName}|]

    exactNodes :: [Record] -> BoltActionT IO [Node]
    exactNodes = mapM (exact . (! _varQNName))


-- | Helper to find 'Relationship's.
-- _varQRName is the mark for this Relationship, which will be used in Cypher queries.
-- For example "RETURN(a)", here _varQRName = "a".
-- RelSelector is used for searching using BoltId-s of 'Node's, connected by this Relationship.
-- RelGraphSelector is used for searching using indexes of 'Node's in the given graph.
data RelSelector = RelSelector { startNodeBoltId :: Maybe BoltId
                               , endNodeBoltId   :: Maybe BoltId
                               , varQRName       :: T.Text
                               , typeS           :: Maybe T.Text
                               }
                 | RelGraphSelector { startNodeIdS :: Maybe Int
                                    , endNodeIdS   :: Maybe Int
                                    , varQRName    :: T.Text
                                    , typeS        :: Maybe T.Text
                                    }

-- | Using the given RelSelector find all 'Relationship's, matching it.
-- This function can't be called "inside" Graph.
getRelationships :: RelSelector -> BoltActionT IO [Relationship]
getRelationships RelSelector{..} = query getQ >>= exactRelationships
  where
    getQ :: T.Text
    getQ = do
      let idStart = maybe "" (T.pack . printf " WHERE ID(a)=%d " . boltId) startNodeBoltId
      let idEnd = maybe "" (T.pack . printf (if idStart == "" then " WHERE ID(b)=%d "
                                                            else " AND ID(b)=%d ") . boltId) endNodeBoltId
      let typeR = maybe "" toCypher typeS
      [text|MATCH (a), (b)
            $idStart $idEnd
            MATCH (a)-[$varQRName $typeR]->(b)
            RETURN $varQRName|]

    exactRelationships :: [Record] -> BoltActionT IO [Relationship]
    exactRelationships = mapM (exact . (! varQRName))

getRelationships RelGraphSelector{..} = error "cant get relationship without graph"


-- | GraphU has edges represented as 'URelationship's (used then creating graph in the database);
-- Graph has edges represented as 'Relationship's (used then getting graph from the database).
data Graph = GraphU { _vertices :: [Node]
                    , _edgesU   :: [URelationship]
                    }
            | Graph { _vertices :: [Node]
                    , _edges    :: [Relationship]
                    } deriving (Show)

-- | The combinations of selectors to load graph from the database.
data GraphSelector = GraphSelector { _verticesSelector      :: [NodeSelector]
                                   , _relationshipsSelector :: [RelSelector]
                                   }


-- | For the given GraphSelector find the graph, which matches it.
-- This function creates single cypher query and performs it.
getGraph :: GraphSelector -> BoltActionT IO Graph
getGraph GraphSelector{..} = do
  res <- query getQ
  let nodes = fmap (foldl' (++) []) (sequenceA $ fmap (\f -> f res) resultNodes)
  let edges = fmap (foldl' (++) []) (sequenceA $ fmap (\f -> f res) resultEdges)
  liftA2 Graph nodes edges
  where
    getQ :: T.Text
    getQ = do
      let nodes = T.intercalate "," $ map (\ns -> do
                                                 let name = _varQNName ns
                                                 let labels = maybe "" toCypher (_labelsS ns)
                                                 [text|($name $labels)|]) _verticesSelector
      let returnNodes = T.intercalate "," $ map (\ns -> do
                                                 let name = _varQNName ns
                                                 [text|$name|]) _verticesSelector
      let conditions = T.intercalate " AND " $ map (\ns -> do
                                    let name = _varQNName ns
                                    let boltIdQ = maybe "" (T.pack . printf "ID(%s)=%d" (T.unpack name) . boltId)
                                                  (_boltIdQ ns)
                                    [text|$boltIdQ|]) _verticesSelector
      let edges = T.intercalate "," $ map (\rs -> do
                                            let name   = varQRName rs
                                            let typeQ  = maybe "" toCypher (typeS rs)
                                            let stNode = maybe "" (_varQNName . (_verticesSelector !!)) (startNodeIdS rs)
                                            let endNode = maybe "" (_varQNName . (_verticesSelector !!)) (endNodeIdS rs)
                                            [text|($stNode)-[$name $typeQ]->($endNode)|]) _relationshipsSelector

      let returnEdges = T.intercalate "," $ map (\rs -> do
                                                 let name = varQRName rs
                                                 [text|$name|]) _relationshipsSelector

      [text|MATCH $nodes
            WHERE $conditions
            MATCH $edges
            RETURN $returnNodes, $returnEdges|]


    resultNodes :: [[Record] -> BoltActionT IO [Node]]
    resultNodes = map (\ns -> mapM (exact . (! _varQNName ns))) _verticesSelector

    resultEdges :: [[Record] -> BoltActionT IO [Relationship]]
    resultEdges = map (\rs -> mapM (exact . (! varQRName rs))) _relationshipsSelector


-- | Create Graph using given GraphU and the list describing 'Node's indices (from the given _vertices),
-- which should be connected by the corresponding 'Relationship'.
-- If there were multiple choices while merging given _vertices, the first match is used for connection.
createGraph :: Graph -> [(Int, Int)] -> BoltActionT IO Graph
createGraph graph rels = do
  nodes <- sequenceA $ fmap mergeNode (_vertices graph)
  edges <- sequenceA $
          map (\i -> do
              let stNode = getBoltId $ head (nodes !! (fst (rels !! i)))
              let endNode = getBoltId $ head (nodes !! (snd (rels !! i)))
              createRelationship stNode ((_edgesU graph) !! i) endNode) [0..(length (_edgesU graph)) - 1]
  return $ Graph (foldl' (++) [] nodes) edges

