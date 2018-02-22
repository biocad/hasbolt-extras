{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Database.Bolt.Extras.Internal.Queries where

import           Control.Monad     (zipWithM)
import           Data.Map.Strict   (toList, (!))
import           Data.Text         as T (Text, concat, cons, intercalate, pack,
                                         snoc, toUpper, unpack)
import           Database.Bolt     (BoltActionT, Node (..), Record,
                                    Relationship (..), URelationship (..),
                                    Value (..), exact, query)
import           NeatInterpolation (text)
import           Text.Printf       (printf)

type Label = Text
type Property = (Text, Value)

class ToQueryText a where
  toQueryText :: a -> Text

instance ToQueryText Label where
  -- | Label with <NAME> for query formatted into ":<NAME>"
  toQueryText = cons ':'

instance ToQueryText [Label] where
  -- | If several labels shoud be selected then we just concat them
  toQueryText = T.concat . map toQueryText

instance ToQueryText Property where
   -- | Converts property with <NAME> and <VALUE> ready to query <NAME>:<VALUE>
  toQueryText (propTitle, value) = T.concat [propTitle, pack ":", valueToText value]
    where
      valueToText :: Value -> Text
      valueToText (N ())     = ""
      valueToText (B bool)   = toUpper . pack . show $ bool
      valueToText (I int)    = pack . show $ int
      valueToText (F double) = pack . show $ double
      valueToText (T t)      = snoc (cons '\"' t) '\"'
      valueToText (L values) = snoc (cons '[' $ intercalate (pack ",") $ map valueToText values) ']'
      valueToText _          = error "Database.Neo4j.Setter.propToText: unacceptable Value type"

instance ToQueryText [Property] where
  toQueryText = T.intercalate (pack ",") . map toQueryText


varQ :: Text
varQ = "n"

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

exactNode :: Record -> BoltActionT IO Node
exactNode record = head <$> exactNodes [record]

exactNodes :: [Record] -> BoltActionT IO [Node]
exactNodes = mapM (exact . (! varQ))

data Entity = NodeEntity Node | URelEntity URelationship

data EntityVar = NodeVar Text | URelVar Text

class EntityLike a where
  toEntity :: a -> Entity
  fromEntity :: Entity -> a

instance EntityLike Node where
  toEntity = NodeEntity

  fromEntity (NodeEntity n) = n
  fromEntity _ = error "Database.Neo4j.Setter.EntityLike: could not unpack entity"

instance EntityLike URelationship where
  toEntity = URelEntity

  fromEntity (URelEntity r) = r
  fromEntity _ = error "Database.Neo4j.Setter.EntityLike: could not unpack entity"

generateEntityVars :: [Entity] -> [EntityVar]
generateEntityVars = zipWith (flip generator) [0..]
  where
    generator :: Entity -> Int -> EntityVar
    generator (NodeEntity _) = NodeVar . pack . printf "N%d"
    generator (URelEntity _) = URelVar . pack . printf "R%d"

varToText :: EntityVar -> Text
varToText (NodeVar n) = n
varToText (URelVar u) = u

-- | Exact Entities by given variable name, which is used to
--   understand where to search 'Entity' and what 'Entity' type is expected.
exactEntity :: EntityVar -> Record -> BoltActionT IO Entity
exactEntity (NodeVar var) record = NodeEntity <$> exact (record ! var)
exactEntity (URelVar var) record = URelEntity . makeU <$> exact (record ! var)
  where
    makeU :: Relationship -> URelationship
    makeU Relationship{..} = URelationship relIdentity relType relProps

-- | Exact sevelar 'Entity' by given 'EntityVar's and 'Record's.
exactEntities :: [EntityVar] -> [Record] -> BoltActionT IO [Entity]
exactEntities = zipWithM exactEntity




data NodeSelector = NodeSelector { idS     :: Maybe Int
                                 , labelsS :: Maybe [Text]
                                 }



getNodes :: NodeSelector -> BoltActionT IO [Node]
getNodes NodeSelector{..} = query getQ >>= exactNodes
  where
    getQ :: Text
    getQ = do
      let idQuard = maybe "" (pack . printf " WHERE ID(%s)=%d " (unpack varQ)) idS
      let labelQuard = maybe "" toQueryText labelsS
      [text|MATCH ($varQ $labelQuard) $idQuard
            RETURN $varQ|]


