{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

module Database.Bolt.Extras.Graph.Internal.Put
  (
    PutNode (..)
  , PutRelationship (..)
  , GraphPutRequest
  , GraphPutResponse
  , putNode
  , putRelationship
  , putGraph
  ) where

import           Control.Monad                                     (forM)
import           Control.Monad.IO.Class                            (MonadIO)
import           Data.Map.Strict                                   (mapWithKey,
                                                                    toList, (!))
import qualified Data.Map.Strict                                   as M (map)
import qualified Data.Text                                         as T (Text,
                                                                         pack)
import           Database.Bolt                                     (BoltActionT,
                                                                    Node (..),
                                                                    RecordValue (..),
                                                                    URelationship (..),
                                                                    Value (..),
                                                                    at, exact,
                                                                    query)
import           Database.Bolt.Extras                              (BoltId, ToCypher (..),
                                                                    fromInt)
import           Database.Bolt.Extras.Graph.Internal.AbstractGraph (Graph (..))
import           NeatInterpolation                                 (text)

type NodeName = T.Text

-- | 'PutNode' is the wrapper for 'Node' where we can specify if we want to merge or create it.
--
data PutNode = BoltId BoltId | MergeN Node | CreateN Node
  deriving (Show)

-- | 'PutRelationship' is the wrapper for 'Relationship' where we can specify if we want to merge or create it.
--
data PutRelationship = MergeR URelationship | CreateR URelationship
  deriving (Show)

-- | The graph of 'Node's with specified uploading type and 'URelationship's.
--
type GraphPutRequest = Graph NodeName PutNode PutRelationship

-- | The graph of 'BoltId's corresponding to the nodes and relationships
-- which we get after putting 'GraphPutRequest'.
--
type GraphPutResponse = Graph NodeName BoltId BoltId

-- | For given @Node _ labels nodeProps@ makes query MERGE or CREATE depending
-- on the type of 'PutNode' and returns 'BoltId' of the loaded 'Node'.
-- If we already know 'BoltId' of the 'Node' with such parameters, this function does nothing.
--
-- Potentially, if you MERGE some 'Node' and its labels and props are occured in
-- several 'Node's, then the result can be not one but several 'Node's,
-- so the result of this function will be a list of corresponding 'BoltId's.
--
putNode :: (MonadIO m) => PutNode -> BoltActionT m [BoltId]
putNode ut = case ut of
    (BoltId bId)   -> pure [bId]
    (MergeN node)  -> helper (T.pack "MERGE") node
    (CreateN node) -> helper (T.pack "CREATE") node
  where
    helper :: (MonadIO m) => T.Text -> Node -> BoltActionT m [BoltId]
    helper q node = do
      let varQ  = "n"

      let labelsQ = toCypher $ labels node
      let propsQ  = toCypher . filter ((/= N ()) . snd) . toList $ nodeProps node

      let getQuery = [text|$q ($varQ $labelsQ {$propsQ})
                           RETURN ID($varQ) as $varQ|]

      records <- query getQuery
      forM records $ \record -> do
        nodeIdentity' <- record `at` varQ >>= exact
        pure $ fromInt nodeIdentity'

-- | Every relationship in Bolt protocol starts from one 'Node' and ends in anoter.
-- For given starting and ending 'Node's 'BoltId's, and for @URelationship  _ urelType urelProps@
-- this method makes MERGE query and then returns the corresponding 'BoltId'.
--
putRelationship :: (MonadIO m) => BoltId -> PutRelationship -> BoltId -> BoltActionT m BoltId
putRelationship start pr end = case pr of
    (MergeR relationship)  -> helper (T.pack "MERGE") relationship
    (CreateR relationship) -> helper (T.pack "CREATE") relationship
  where
    helper :: (MonadIO m) => T.Text -> URelationship -> BoltActionT m BoltId
    helper q URelationship{..} = do
        [record]      <- query putQuery
        urelIdentity' <- record `at` varQ >>= exact
        pure $ fromInt urelIdentity'
      where
        varQ   = "r"
        labelQ = toCypher urelType
        propsQ = toCypher . toList $ urelProps
        startT = T.pack . show $ start
        endT   = T.pack . show $ end

        putQuery :: T.Text
        putQuery = [text|MATCH (a), (b)
                         WHERE ID(a) = $startT AND ID(b) = $endT
                         $q (a)-[$varQ $labelQ {$propsQ}]->(b)
                         RETURN ID($varQ) as $varQ|]

-- | Creates graph using given 'GraphPutRequest'.
-- If there were multiple choices while merging given _vertices, the first match is used for connection.
--
putGraph :: (MonadIO m) => GraphPutRequest -> BoltActionT m GraphPutResponse
putGraph requestGraph = do
  let vertices' = _vertices requestGraph
  let rels      = _relations requestGraph
  nodes <- sequenceA $ M.map (fmap head . putNode) vertices'
  edges <- sequenceA $
          mapWithKey (\key v -> do
              let stNode  = nodes ! fst key
              let endNode = nodes ! snd key
              putRelationship stNode v endNode) rels
  return $ Graph nodes edges
