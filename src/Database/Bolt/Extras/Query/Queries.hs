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
  ) where

import           Control.Monad                     (forM)
import           Control.Monad.IO.Class            (MonadIO)
import           Data.Map.Strict                   (toList, (!))
import           Data.Text                         as T (Text, pack, unpack)
import           Database.Bolt                     (BoltActionT, Node (..),
                                                    Record, URelationship (..),
                                                    at, exact, query)
import           Database.Bolt.Extras.Query.Cypher (ToCypher (..))
import           Database.Bolt.Extras.Query.Entity (EntityLike (..),
                                                    generateEntityVars)
import           Database.Bolt.Id                  (BoltId (..))
import           NeatInterpolation                 (text)
import           Text.Printf                       (printf)

-- | For given @Node _ labels nodeProps@ makes query @MERGE (n:labels {props}) RETURN ID(n) as n@
-- and then return 'Node' with actual ID.
--
-- Potentially, if you MERGE some 'Node' and it labels and props are occured in
-- several 'Node's, then the result can be not one but several 'Node's.
--
mergeNode :: MonadIO m => Node -> BoltActionT m [Node]
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

    mergeQ :: Text
    mergeQ = [text|MERGE ($varQ $labelsQ {$propsQ})
                   RETURN ID($varQ) as $varQ|]

-- | Every relationship in Bolt protocol starts from one 'Node' and ends in anoter.
-- For given starting and ending 'Node's, and for @URelationship  _ urelType urelProps@
-- this method makes MERGE query and then return 'URelationship' with actual ID.
createRelationship :: MonadIO m => BoltId -> URelationship -> BoltId -> BoltActionT m URelationship
createRelationship start urel@URelationship{..} end = do
  [record]      <- query mergeQ
  urelIdentity' <- record `at` varQ >>= exact
  pure $ urel {urelIdentity = urelIdentity'}
  where
    [var] = generateEntityVars [toEntity urel]
    varQ = toCypher var

    mergeQ :: Text
    mergeQ = do
      let labelQ = toCypher urelType
      let propsQ = toCypher . toList $ urelProps
      let startT = pack . show . boltId $ start
      let endT = pack . show . boltId $ end

      [text|MATCH (a), (b)
            WHERE ID(a) = $startT AND ID(b) = $endT
            MERGE (a)-[$varQ $labelQ {$propsQ}]->(b)
            RETURN ID($varQ) as $varQ|]

getNodes :: NodeSelector -> BoltActionT IO [Node]
getNodes NodeSelector{..} = query getQ >>= exactNodes
  where
    varQ :: Text
    varQ = "n"

    getQ :: Text
    getQ = do
      let idQuard = maybe "" (pack . printf " WHERE ID(%s)=%d " (unpack varQ)) idS
      let labelQuard = maybe "" toCypher labelsS
      [text|MATCH ($varQ $labelQuard) $idQuard
            RETURN $varQ|]

    exactNodes :: [Record] -> BoltActionT IO [Node]
    exactNodes = mapM (exact . (! varQ))

data NodeSelector = NodeSelector { idS     :: Maybe Int
                                 , labelsS :: Maybe [Text]
                                 }





