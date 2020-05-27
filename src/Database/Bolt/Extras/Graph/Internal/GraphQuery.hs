{-# LANGUAGE AllowAmbiguousTypes     #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE OverloadedStrings       #-}
{-# LANGUAGE QuasiQuotes             #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE TypeApplications        #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE TypeFamilyDependencies  #-}

module Database.Bolt.Extras.Graph.Internal.GraphQuery
  (
    GraphQuery (..)
  , GetRequest (..)
  , PutRequest (..)
  , mergeGraphs
  ) where

import           Control.Lens                                      (over, (^.))
import           Control.Monad.IO.Class                            (MonadIO)
import           Data.List                                         (foldl')
import           Data.Map.Strict                                   (fromList,
                                                                    mapKeys,
                                                                    mapWithKey,
                                                                    toList,
                                                                    union, (!))
import           Data.Text                                         as T (Text, intercalate,
                                                                         null,
                                                                         pack)
import           Database.Bolt                                     (BoltActionT,
                                                                    Record,
                                                                    query)
import           Database.Bolt.Extras                              (BoltId, GetBoltId (..))
import           Database.Bolt.Extras.Graph.Internal.AbstractGraph (Graph (..),
                                                                    NodeName,
                                                                    emptyGraph,
                                                                    relationName,
                                                                    relations,
                                                                    vertices)
import           Database.Bolt.Extras.Graph.Internal.Class         (Extractable (..),
                                                                    Requestable (..),
                                                                    Returnable (..))
import           Database.Bolt.Extras.Graph.Internal.Get           (NodeGetter,
                                                                    NodeResult,
                                                                    RelGetter,
                                                                    RelResult,
                                                                    requestGetters)
import           Database.Bolt.Extras.Graph.Internal.Put           (PutNode, PutRelationship,
                                                                    requestPut)
import           NeatInterpolation                                 (text)

-- | Type class used to perform requests to the Neo4j based on graphs.
--
class GraphQuery a where
  -- | Type of entity describing node for request.
  type NodeReq a :: *
  -- | Type of entity describing relationship for request.
  type RelReq  a :: *
  -- | Type of node entity which will be extracted from result.
  type NodeRes a :: *
  -- | Type of relationship entity which will be extracted from result.
  type RelRes  a :: *

  -- | Convert requestable entities to text in the query.
  requestEntities :: (Requestable (NodeName, NodeReq a),
                      Requestable ((NodeName, NodeName), RelReq a))
                  => [(NodeName, NodeReq a)]
                  -> [((NodeName, NodeName), RelReq a)]
                  -> (Text, [Text])

  -- | Abstract function to form query for request.
  --
  formQuery :: (Requestable (NodeName, NodeReq a),
                Requestable ((NodeName, NodeName), RelReq a),
                Returnable (NodeName, NodeReq a),
                Returnable ((NodeName, NodeName), RelReq a))
            => [Text]                                -- ^ Custom conditions that will be added to @WHERE@ block.
            -> Graph NodeName (NodeReq a) (RelReq a) -- ^ Request graph template.
            -> Text                                  -- ^ Cypher query as text.
  formQuery customConds graph = [text|$completeRequest
                                      $conditionsQ
                                      WITH DISTINCT $distinctVars
                                      RETURN $completeReturn|]
    where
      vertices'        = toList (graph ^. vertices)
      relations'       = toList (graph ^. relations)
      distinctVars     = intercalate ", " $ fmap fst vertices' ++ fmap (relationName . fst) relations'

      (completeRequest, reqConds) = requestEntities @a vertices' relations'

      conditions       = reqConds ++ customConds
      conditionsQ      = if Prelude.null conditions then "" else " WHERE " <> intercalate " AND " conditions

      returnVertices   = return' <$> filter isReturned' vertices'
      returnRelations  = return' <$> filter isReturned' relations'

      completeReturn   = intercalate ", " $ Prelude.filter (not . T.null) $ returnVertices ++ returnRelations

  -- | Abstract function which exctracts graph from records if nodes and relations can be extracted.
  --
  extractGraphs :: (Extractable (NodeRes a), Extractable (RelRes a), MonadIO m)
                => [NodeName]
                -> [(NodeName, NodeName)]
                -> [Record]
                -> BoltActionT m [Graph NodeName (NodeRes a) (RelRes a)]
  extractGraphs verticesN relationsN records = mapM (\i -> do
        vertices'  <- zip verticesN  <$> traverse (fmap (!! i) . flip extract records               ) verticesN
        relations' <- zip relationsN <$> traverse (fmap (!! i) . flip extract records . relationName) relationsN
        pure $ Graph (fromList vertices') (fromList relations'))
      [0 .. length records - 1]

  -- | For given query graph, perform query and extract results graph.
  --
  makeRequest :: (Requestable (NodeName, NodeReq a),
                  Requestable ((NodeName, NodeName), RelReq a),
                  Returnable (NodeName, NodeReq a),
                  Returnable ((NodeName, NodeName), RelReq a),
                  Extractable (NodeRes a),
                  Extractable (RelRes a),
                  MonadIO m)
              => [Text]
              -> Graph NodeName (NodeReq a) (RelReq a)
              -> BoltActionT m [Graph NodeName (NodeRes a) (RelRes a)]
  makeRequest conds graph = do
      response <- query $ formQuery @a conds graph
      extractGraphs @a presentedVertices presentedRelations response
    where
      presentedVertices  = fmap fst . filter isReturned' . toList $ graph ^. vertices
      presentedRelations = fmap fst . filter isReturned' . toList $ graph ^. relations

---------------------------------------------------------------------------------------
-- GET --
---------------------------------------------------------------------------------------

-- | Get request with graph result.
--
data GetRequest = GetRequest

instance GraphQuery GetRequest where
  type NodeReq GetRequest = NodeGetter
  type RelReq  GetRequest = RelGetter
  type NodeRes GetRequest = NodeResult
  type RelRes  GetRequest = RelResult
  requestEntities         = requestGetters

---------------------------------------------------------------------------------------
-- PUT --
---------------------------------------------------------------------------------------

-- | Put request in Bolt format with 'BoltId's of uploaded entities as result.
--
data PutRequest = PutRequest

instance GraphQuery PutRequest where
  type NodeReq PutRequest = PutNode
  type RelReq  PutRequest = PutRelationship
  type NodeRes PutRequest = BoltId
  type RelRes  PutRequest = BoltId
  requestEntities          = requestPut

-- | Helper function to merge graphs of results, i.e.
-- if you requested graph @A -> B -> C@
-- and in the database there were two @B@ entities connected to the same entity @A@
-- and four @C@ entities connected to the same two entities @B@,
-- Cypher query will return four graphs which satisfy this path,
-- despite the fact that @A@ was present only once in the database
-- and @B@ was present only two times in the database.
--
-- This function will merge these four graphs in one
-- and return nodes by node names with suffixes equal to their 'BoltId's.
--
-- For example, if there were four graphs:
--
-- @
--   nodes: [A (boltId = 0), B (boltId = 1), C (boltId = 3)], relations: [A -> B, B -> C];
--   nodes: [A (boltId = 0), B (boltId = 1), C (boltId = 4)], relations: [A -> B, B -> C];
--   nodes: [A (boltId = 0), B (boltId = 2), C (boltId = 5)], relations: [A -> B, B -> C];
--   nodes: [A (boltId = 0), B (boltId = 2), C (boltId = 6)], relations: [A -> B, B -> C].
-- @
-- this function will merge them into new graph:
--
-- @
--   nodes: [A0 (boltId = 0), B1 (boltId = 1), B2 (boltId = 2),
--           C3 (boltId = 3), C4 (boltId = 4), C5 (boltId = 5), C6 (boltId = 6)],
--   relations: [A0 -> B1, A0 -> B2, B1 -> C3, B1 -> C4, B2 -> C5, B2 -> C6].
-- @
--
mergeGraphs :: GetBoltId a => [Graph NodeName a b] -> Graph NodeName a b
mergeGraphs graphs = foldl' mergeGraph emptyGraph (updateGraph <$> graphs)
  where
    updateGraph :: GetBoltId a => Graph NodeName a b -> Graph NodeName a b
    updateGraph graph = Graph newVertices newRelations
      where
        namesMap     = (\name        node     ->  name <> (pack . show . getBoltId $ node)  ) `mapWithKey` (graph ^. vertices)
        newVertices  = (\name                 ->  namesMap ! name                           ) `mapKeys`    (graph ^. vertices)
        newRelations = (\(startName, endName) -> (namesMap ! startName, namesMap ! endName) ) `mapKeys`    (graph ^. relations)

    mergeGraph :: GetBoltId a => Graph NodeName a b -> Graph NodeName a b -> Graph NodeName a b
    mergeGraph graphToMerge initialGraph = over relations (union (graphToMerge ^. relations)) $
                                           over vertices  (union (graphToMerge ^. vertices))
                                           initialGraph
