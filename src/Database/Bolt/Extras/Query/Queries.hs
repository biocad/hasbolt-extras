{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Database.Bolt.Extras.Query.Queries
  ( uploadNode
  , createRelationship
  , getNodes
  , getRelationships
  , UploadTypedNode (..)
  , getNode
  ) where

import           Control.Monad                        (forM)
import           Control.Monad.IO.Class               (MonadIO)
import           Data.Map.Strict                      (toList, (!))
import qualified Data.Text                            as T (Text, pack, concat)
import           Database.Bolt                        (BoltActionT, Node (..),
                                                       Record, RecordValue (..),
                                                       Relationship (..),
                                                       URelationship (..), at,
                                                       exact, query)
import           Database.Bolt.Extras.Query.Cypher    (ToCypher (..))
import           Database.Bolt.Extras.Query.Selectors (NodeSelector (..),
                                                       RelSelector (..))
import           Database.Bolt.Id                     (BoltId (..), fromInt)
import           NeatInterpolation                    (text)
import           Text.Printf                          (printf)
-- | For given @Node _ labels nodeProps@ makes query @MERGE (n:labels {props}) RETURN ID(n) as n@
-- and then return 'Node' with actual ID.
--
-- Potentially, if you MERGE some 'Node' and it labels and props are occured in
-- several 'Node's, then the result can be not one but several 'Node's.
--

data UploadTypedNode = BoltId BoltId | Merge Node | Create Node
  deriving (Show)

getNode :: UploadTypedNode -> Node
getNode (Merge node)  = node
getNode (Create node) = node
getNode (BoltId _)    = undefined

uploadNode :: (MonadIO m) => UploadTypedNode -> BoltActionT m [BoltId]
uploadNode ut = case ut of
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
createRelationship :: (MonadIO m) => BoltId -> URelationship -> BoltId -> BoltActionT m BoltId
createRelationship start URelationship{..} end = do
  [record]      <- query mergeQ
  urelIdentity' <- record `at` varQ >>= exact
  pure $ fromInt urelIdentity'
  where
    varQ = "r"

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

-- | Using the given NodeSelector returns the list of all 'Node's, matching it.
getNodes :: (MonadIO m) => NodeSelector -> BoltActionT m [Node]
getNodes NodeSelector{..} = query getQ >>= exactNodes
  where
    varQ :: T.Text
    varQ = "n"

    getQ :: T.Text
    getQ = do
      let idQuard = maybe "" (T.pack . printf " WHERE ID(%s)=%d " varQ . boltId) boltIdQ
      let labelQuard = maybe "" toCypher labelsS
      let propsQ = maybe "" (\props -> T.concat ["{", toCypher props, "}"]) propsS
      [text|MATCH ($varQ $labelQuard $propsQ) $idQuard
            RETURN $varQ|]

    exactNodes :: (MonadIO m) => [Record] -> BoltActionT m [Node]
    exactNodes = mapM (exact . (! varQ))


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
      let propsQ = maybe "" toCypher propsRS
      [text|MATCH (a), (b)
            $idStart $idEnd
            MATCH (a)-[$varQ $typeR {$propsQ}]->(b)
            RETURN $varQ|]

    exactRelationships :: (MonadIO m) => [Record] -> BoltActionT m [Relationship]
    exactRelationships = mapM (exact . (! varQ))
