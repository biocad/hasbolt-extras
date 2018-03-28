{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Database.Bolt.Extras.Query.Put
    ( GraphPutRequest
    , GraphPutResponse
    , PutNode (..)
    , putGraph
    ) where

import           Control.Monad                     (forM)
import           Control.Monad.IO.Class            (MonadIO)
import           Data.Map.Strict                   (mapWithKey, toList, (!))
import qualified Data.Map.Strict                   as M (map)
import qualified Data.Text                         as T (Text, pack)
import           Database.Bolt                     (BoltActionT, Node (..),
                                                    RecordValue (..),
                                                    URelationship (..), at,
                                                    exact, query)
import           Database.Bolt.Extras.Graph        (Graph (..))
import           Database.Bolt.Extras.Persisted    (BoltId, fromInt)
import           Database.Bolt.Extras.Query.Cypher (ToCypher (..))
import           Database.Bolt.Extras.Query.Utils  (NodeName)
import           NeatInterpolation                 (text)

-- | For given @Node _ labels nodeProps@ makes query @MERGE (n:labels {props}) RETURN ID(n) as n@
-- and then return 'Node' with actual ID.
--
-- Potentially, if you MERGE some 'Node' and it labels and props are occured in
-- several 'Node's, then the result can be not one but several 'Node's.
--
data PutNode = BoltId BoltId | Merge Node | Create Node
  deriving (Show)

type GraphPutRequest = Graph NodeName PutNode URelationship

type GraphPutResponse = Graph NodeName BoltId BoltId

putNode :: (MonadIO m) => PutNode -> BoltActionT m [BoltId]
putNode ut = case ut of
    (BoltId bId)  -> pure [bId]
    (Merge node)  -> helper (T.pack "MERGE") node
    (Create node) -> helper (T.pack "CREATE") node
  where
    helper :: (MonadIO m) => T.Text -> Node -> BoltActionT m [BoltId]
    helper q node = do
      let varQ  = "n"

      let labelsQ = toCypher $ labels node
      let propsQ  = toCypher . toList $ nodeProps node

      let getQuery = [text|$q ($varQ $labelsQ {$propsQ})
                           RETURN ID($varQ) as $varQ|]

      records <- query getQuery
      forM records $ \record -> do
        nodeIdentity' <- record `at` varQ >>= exact
        pure $ fromInt nodeIdentity'

-- | Every relationship in Bolt protocol starts from one 'Node' and ends in anoter.
-- For given starting and ending 'Node's, and for @URelationship  _ urelType urelProps@
-- this method makes MERGE query and then return 'Relationship' with actual ID.
putRelationship :: (MonadIO m) => BoltId -> URelationship -> BoltId -> BoltActionT m BoltId
putRelationship start URelationship{..} end = do
  [record]      <- query mergeQ
  urelIdentity' <- record `at` varQ >>= exact
  pure $ fromInt urelIdentity'
  where
    varQ = "r"
    labelQ = toCypher urelType
    propsQ = toCypher . toList $ urelProps
    startT = T.pack . show $ start
    endT = T.pack . show $ end

    mergeQ :: T.Text
    mergeQ = [text|MATCH (a), (b)
              WHERE ID(a) = $startT AND ID(b) = $endT
              MERGE (a)-[$varQ $labelQ {$propsQ}]->(b)
              RETURN ID($varQ) as $varQ|]

-- | Create Graph using given GraphU and the list describing 'Node's indices (from the given _vertices),
-- which should be connected by the corresponding 'Relationship'.
-- If there were multiple choices while merging given _vertices, the first match is used for connection.
putGraph :: (MonadIO m) => GraphPutRequest -> BoltActionT m GraphPutResponse
putGraph requestGraph = do
  let vertices = _vertices requestGraph
  let rels = _relations requestGraph
  nodes <- sequenceA $ M.map (fmap head . putNode) vertices
  edges <- sequenceA $
          mapWithKey (\key v -> do
              let stNode  = nodes ! fst key
              let endNode = nodes ! snd key
              putRelationship stNode v endNode) rels
  return $ Graph nodes edges
