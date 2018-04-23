{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Database.Bolt.Extras.Query.Delete
  (
    deleteNodes
  ) where

import           Control.Monad                  (forM)
import           Control.Monad.IO.Class         (MonadIO)
import           Data.Text                      (Text, intercalate, pack)
import           Database.Bolt                  (BoltActionT, RecordValue (..),
                                                 at, exact, query)
import           Database.Bolt.Extras.Persisted (BoltId, fromInt)
import           NeatInterpolation              (text)

-- | 'deleteNodes' is used to delete all nodes with given 'BoltId's
-- and all corresponding relatioships.
--
deleteNodes :: (MonadIO m) => [BoltId] -> BoltActionT m [BoltId]
deleteNodes boltIds = do
    records <- query formQuery
    forM records $ \record -> do
      nodeIdentity' <- record `at` varQ >>= exact
      pure $ fromInt nodeIdentity'
  where
    varQ = "n"

    formBoltIds :: Text
    formBoltIds = intercalate ", " $ fmap (pack . show) boltIds

    formQuery = [text|MATCH ($varQ)
                      WHERE ID($varQ) IN [$formBoltIds]
                      DETACH DELETE $varQ
                      RETURN ID($varQ) as $varQ|]
