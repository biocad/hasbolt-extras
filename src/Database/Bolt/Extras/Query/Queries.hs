{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TemplateHaskell      #-}

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
  , ListSelector (..)
  , GraphSelector (..)
  , Graph (..)
  , GraphU (..)
  , verticesU
  , edgesU
  , rels
  ) where

import           Control.Monad.IO.Class            (MonadIO)
import           Control.Applicative               (liftA2)
import           Control.Monad                     (forM)
import           Data.List                         (foldl')
import           Data.Map.Strict                   (fromList, toList, (!))
import qualified Data.Text                         as T (Text, intercalate,
                                                         pack)
import           Database.Bolt                     (BoltActionT, Node (..),
                                                    Record, Relationship (..),
                                                    URelationship (..), at,
                                                    exact, query,
                                                    RecordValue (..))
import           Database.Bolt.Extras.Query.Cypher (ToCypher (..))
import           Database.Bolt.Extras.Query.Entity (EntityLike (..),
                                                    generateEntityVars)
import           Database.Bolt.Id                  (BoltId (..), GetBoltId (..), fromInt)
import           NeatInterpolation                 (text)
import           Text.Printf                       (printf)
import           Debug.Trace
import           Data.Maybe                        (fromMaybe)
import           Control.Lens                      (makeLenses)
-- | For given @Node _ labels nodeProps@ makes query @MERGE (n:labels {props}) RETURN ID(n) as n@
-- and then return 'Node' with actual ID.
--
-- Potentially, if you MERGE some 'Node' and it labels and props are occured in
-- several 'Node's, then the result can be not one but several 'Node's.
--
mergeNode :: (MonadIO m) => Node -> BoltActionT m [Node]
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
createRelationship :: (MonadIO m) => BoltId -> URelationship -> BoltId -> BoltActionT m Relationship
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
                                 , _labelsS   :: Maybe [T.Text]
                                 } deriving (Show)

-- | Using the given NodeSelector returns the list of all 'Node's, matching it.
getNodes :: (MonadIO m) => NodeSelector -> BoltActionT m [Node]
getNodes NodeSelector{..} = query getQ >>= exactNodes
  where
    varQ :: T.Text
    varQ = "n"

    getQ :: T.Text
    getQ = do
      let idQuard = maybe "" (T.pack . printf " WHERE ID(%s)=%d " varQ . boltId) _boltIdQ
      let labelQuard = maybe "" toCypher _labelsS
      [text|MATCH ($varQ $labelQuard) $idQuard
            RETURN $varQ|]

    exactNodes :: (MonadIO m) => [Record] -> BoltActionT m [Node]
    exactNodes = mapM (exact . (! varQ))


-- | Helper to find 'Relationship's.
-- _varQRName is the mark for this Relationship, which will be used in Cypher queries.
-- For example "RETURN(a)", here _varQRName = "a".
-- RelSelector is used for searching using BoltId-s of 'Node's, connected by this Relationship.
data RelSelector = RelSelector { startNodeBoltId :: Maybe BoltId
                               , endNodeBoltId   :: Maybe BoltId
                               , typeS           :: Maybe T.Text
                               } deriving (Show)

-- | ListSelector is used for searching using indexes of 'Node's in the given graph.
data ListSelector = ListSelector { startNodeIdS :: Maybe Int
                                 , endNodeIdS   :: Maybe Int
                                 , typeLS        :: Maybe T.Text
                                 } deriving (Show) 

-- | Using the given RelSelector find all 'Relationship's, matching it.
getRelationships :: (MonadIO m) => RelSelector -> BoltActionT m [Relationship]
getRelationships RelSelector{..} = query getQ >>= exactRelationships
  where
    varQ :: T.Text
    varQ = "r"

    getQ :: T.Text
    getQ = do
      let idStart = maybe "" (T.pack . printf " WHERE ID(a)=%d " . boltId) startNodeBoltId
      let idEnd = maybe "" (T.pack . printf (if idStart == "" then " WHERE ID(b)=%d "
                                                            else " AND ID(b)=%d ") . boltId) endNodeBoltId
      let typeR = maybe "" toCypher typeS
      [text|MATCH (a), (b)
            $idStart $idEnd
            MATCH (a)-[$varQ $typeR]->(b)
            RETURN $varQ|]

    exactRelationships :: (MonadIO m) => [Record] -> BoltActionT m [Relationship]
    exactRelationships = mapM (exact . (! varQ))


-- | GraphU has edges represented as 'URelationship's (used then creating graph in the database);
-- Graph has edges represented as 'Relationship's (used then getting graph from the database).
data GraphU = GraphU { _verticesU :: [Node]
                     , _edgesU    :: [URelationship]
                     , _rels      :: [(Int, Int)]
                     }
data Graph = Graph { _vertices :: [Node]
                   , _edges    :: [Relationship]
                   } deriving (Show)

-- | The combinations of selectors to load graph from the database.
data GraphSelector = GraphSelector { _verticesSelector      :: [NodeSelector]
                                   , _listSelector :: [ListSelector]
                                   } deriving (Show)



makeLenses ''GraphU

-- | For the given GraphSelector find the graph, which matches it.
-- This function creates single cypher query and performs it.
getGraph :: (MonadIO m) => GraphSelector -> BoltActionT m Graph
getGraph GraphSelector{..} = do
  res <- query getQ
  let nodes = fmap (foldl' (++) []) (exactValues nodeVars res)
  let edges = fmap (foldl' (++) []) (exactValues edgesVars res)
  liftA2 Graph nodes edges
  where

    nodeVars :: [T.Text]
    nodeVars = map toCypher $ generateEntityVars (map toEntity
                  [ (Node i lbl (fromList [])) | vs <- _verticesSelector,
                                                 let i = (boltId . fromMaybe (fromInt (-1)) . _boltIdQ) vs,
                                                 let lbl = (fromMaybe [] . _labelsS) vs])
    
    edgesVars :: [T.Text]
    edgesVars = map toCypher $ generateEntityVars (map toEntity 
                  [ (URelationship (-1) tp (fromList [])) | tp <- map (fromMaybe "" . typeLS) _listSelector])

    getQ :: T.Text
    getQ = do
      let nodes = T.intercalate "," $ map (\i -> do
                                            let name = nodeVars !! i
                                            let labels = maybe "" toCypher (_labelsS (_verticesSelector !! i))
                                            [text|($name $labels)|]) [0..(length _verticesSelector - 1)]
      let returnNodes = trace (show nodeVars) $ T.intercalate "," $ nodeVars
      let conditions = (T.intercalate " AND " . filter (/= "\n")) $ map (\i -> do
                                    let name = nodeVars !! i
                                    let boltIdQ = maybe "" (T.pack . printf "ID(%s)=%d" name . boltId)
                                                  (_boltIdQ $ _verticesSelector !! i)
                                    [text|$boltIdQ|]) [0..(length _verticesSelector - 1)]
      let edges = T.intercalate "," $ map (\i -> do
                                            let name   = edgesVars !! i
                                            let rs     = _listSelector !! i
                                            let typeQ  = maybe "" toCypher (typeLS rs)
                                            let stNode = maybe "" (nodeVars !!) (startNodeIdS rs)
                                            let stNodeLabels = maybe "" (maybe "" toCypher . _labelsS . (_verticesSelector !!)) (startNodeIdS rs)
                                            let endNode = maybe "" (nodeVars !!) (endNodeIdS rs)
                                            let endNodeLabels = maybe "" (maybe "" toCypher . _labelsS . (_verticesSelector !!)) (endNodeIdS rs)
                                            [text|($stNode $stNodeLabels)-[$name $typeQ]-($endNode $endNodeLabels)|])
                                          [0..(length _listSelector - 1)]

      let returnEdges = T.intercalate "," $ edgesVars

      [text|MATCH $nodes
            WHERE $conditions
            MATCH $edges
            RETURN $returnNodes, $returnEdges|]



    exactValues :: (MonadIO m, RecordValue a) => [T.Text] -> [Record] -> BoltActionT m [[a]]
    exactValues vars = mapM (\record -> mapM (\var -> exact (record ! var) ) vars)

-- | Create Graph using given GraphU and the list describing 'Node's indices (from the given _vertices),
-- which should be connected by the corresponding 'Relationship'.
-- If there were multiple choices while merging given _vertices, the first match is used for connection.
createGraph :: (MonadIO m) => GraphU -> BoltActionT m Graph
createGraph GraphU{..} = do
  nodes <- sequenceA $ fmap mergeNode _verticesU
  edges <- sequenceA $
          map (\i -> do
              let stNode = getBoltId $ head (nodes !! (fst (_rels !! i)))
              let endNode = getBoltId $ head (nodes !! (snd (_rels !! i)))
              createRelationship stNode (_edgesU !! i) endNode) [0..(length _edgesU) - 1]
  return $ Graph (foldl' (++) [] nodes) edges

