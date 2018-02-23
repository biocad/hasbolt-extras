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
                                                    Value (..), exact, query)
import           NeatInterpolation                 (text)
import           Text.Printf                       (printf)

import           Database.Bolt.Extras.Query.Entity
import           Database.Bolt.Extras.Template
{--------------------------------------------------------------------
  Setters
--------------------------------------------------------------------}
setNode :: Node -> BoltActionT IO Node
setNode node@Node{..} = do
  [record] <- query mergeQ
  fromEntity <$> exactEntity var record
  where
    [var] = generateEntityVars [toEntity node]
    varQ  = varToText var

    labelsQ = toQueryText labels
    propsQ  = T.intercalate (pack ",") . map toQueryText . toList $ nodeProps

    mergeQ :: Text
    mergeQ = [text|MERGE ($varQ $labelsQ {$propsQ})
                   RETURN $varQ|]

setRelationship :: Int -> Int -> URelationship -> BoltActionT IO URelationship
setRelationship startNodeIdx endNodeIdx urel@URelationship{..} = do
  [record] <- query mergeQ
  fromEntity <$> exactEntity var record
  where
    [var] = generateEntityVars [toEntity urel]
    varQ = varToText var
    mergeQ :: Text
    mergeQ = do
      let labelQ = toQueryText urelType
      let propsQ = T.intercalate (pack ",") . map toQueryText . toList $ urelProps
      let startT = pack . show $ startNodeIdx
      let endT = pack . show $ endNodeIdx
      [text|MATCH (a), (b)
            WHERE ID(a) = $startT AND ID(b) = $endT
            MERGE (a)-[$varQ $labelQ {$propsQ}]->(b)
            RETURN $varQ|]

{--------------------------------------------------------------------
  Getters
--------------------------------------------------------------------}

getNodes :: NodeSelector -> BoltActionT IO [Node]
getNodes NodeSelector{..} = query getQ >>= exactNodes
  where
    varQ :: Text
    varQ = "n"

    getQ :: Text
    getQ = do
      let idQuard = maybe "" (pack . printf " WHERE ID(%s)=%d " (unpack varQ)) idS
      let labelQuard = maybe "" toQueryText labelsS
      [text|MATCH ($varQ $labelQuard) $idQuard
            RETURN $varQ|]

    exactNodes :: [Record] -> BoltActionT IO [Node]
    exactNodes = mapM (exact . (! varQ))

{--------------------------------------------------------------------
  Selectors
--------------------------------------------------------------------}

data NodeSelector = NodeSelector { idS     :: Maybe Int
                                 , labelsS :: Maybe [Text]
                                 }

{--------------------------------------------------------------------
 Internal
--------------------------------------------------------------------}

{--------------------------------------------------------------------
  Types
--------------------------------------------------------------------}

-- | The class to convert anything to ready-query representation.
class ToQueryText a where
  toQueryText :: a -> Text

instance ToQueryText Label where
  -- | Label with @name@ for query formatted into @:name@
  toQueryText = cons ':'

instance ToQueryText [Label] where
  -- | If several labels should be formatted then we just concat them
  toQueryText = T.concat . map toQueryText

instance ToQueryText Property where
   -- | Converts property (@('Text', 'Value')@) with @name@ and @value@ to query-ready @name:value@
  toQueryText (propTitle, value) = T.concat [propTitle, pack ":", valueToText value]
    where
      valueToText :: Value -> Text
      valueToText (N ())     = ""
      valueToText (B bool)   = toUpper . pack . show $ bool
      valueToText (I int)    = pack . show $ int
      valueToText (F double) = pack . show $ double
      valueToText (T t)      = T.concat ["\"", t,"\""]
      valueToText (L values) = T.concat ["[", T.intercalate "," $ map valueToText values,"]"]
      valueToText _          = error "Database.Neo4j.Setter.propToText: unacceptable Value type"

instance ToQueryText [Property] where
  -- | If several properties should be formatted then we just concat them
  toQueryText = T.intercalate "," . map toQueryText







