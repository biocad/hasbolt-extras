{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Database.Bolt.Extras.Query.Set
  (
    setNode
  ) where

import           Control.Monad.IO.Class              (MonadIO)
import           Data.Monoid                         ((<>))
import           Data.Text                           (Text, intercalate)
import           Database.Bolt                       (BoltActionT,
                                                      RecordValue (..),
                                                      Value (..), at, exact,
                                                      query)
import           Database.Bolt.Extras.Persisted      (BoltId, fromInt)
import           Database.Bolt.Extras.Query.Cypher   (ToCypher (..))
import           Database.Bolt.Extras.Query.Get      (NodeGetter, condIdAsText,
                                                      nodeAsText)
import           Database.Bolt.Extras.Template.Types (Property)
import           NeatInterpolation                   (text)

-- | 'setNode' updates properties for the node,
-- corresponding to the given 'NodeGetter'.
--
setNode :: (MonadIO m) => NodeGetter -> [Property] -> BoltActionT m BoltId
setNode nodeGetter props = do
    let nodeGetterT = nodeAsText (varQ, nodeGetter)
    let condId      = condIdAsText (varQ, nodeGetter)

    let newProperties = intercalate "," $ formPropertySet <$> filter ((/= N ()) . snd) props

    let getQuery = [text|MATCH $nodeGetterT
                         WHERE $condId
                         SET $newProperties
                         RETURN ID($varQ) as $varQ|]

    record <- head <$> query getQuery
    nodeIdentity' <- record `at` varQ >>= exact
    pure $ fromInt nodeIdentity'

  where
    varQ = "n"

    formPropertyName :: Text -> Text
    formPropertyName n = varQ <> "." <> n

    formPropertySet :: Property -> Text
    formPropertySet (name, prop) = formPropertyName name <> "=" <> toCypher prop
