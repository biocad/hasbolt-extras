{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Database.Bolt.Extras.Query.Queries where

import           Data.Map.Strict                   (toList, (!))
import           Data.Text                         as T (Text, concat, cons,
                                                         intercalate, pack,
                                                         toUpper, unpack)
import           Database.Bolt                     (BoltActionT, Node (..),
                                                    Record, URelationship (..),
                                                    Value (..), exact, at, query)
import           Database.Bolt.Extras.Query.Cypher (ToCypher (..))
import           Database.Bolt.Extras.Query.Entity
import           Database.Bolt.Extras.Template
import Control.Monad (forM)
import           NeatInterpolation                 (text)
import           Text.Printf                       (printf)
import Debug.Trace (trace)

-- | For given @Node _ labels nodeProps@ makes query @MERGE (n:labels {props}) RETURN ID(n) as n@
-- and then return 'Node' with actual ID.
--
-- Potentially, if you MERGE some 'Node' and it labels and props are occured in
-- several 'Node's, then the result can be not one but several 'Node's.
--
mergeNode :: Node -> BoltActionT IO [Node]
mergeNode node@Node{..} = do
  records      <- trace (unpack mergeQ) $ query mergeQ
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
mergeRelationship :: Node -> Node -> URelationship -> BoltActionT IO URelationship
mergeRelationship startNode endNode urel@URelationship{..} = do
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
      let startT = pack . show . nodeIdentity $ startNode
      let endT = pack . show . nodeIdentity $ endNode
      
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





