{-# LANGUAGE DeriveFunctor   #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Database.Bolt.Extras.Persisted
  (
    BoltId
  , Persisted (..)
  , GetBoltId (..)
  , fromInt
  ) where

import           Data.Aeson                 (FromJSON (..), ToJSON (..),
                                             genericParseJSON, genericToJSON)
import           Data.Aeson.Casing          (aesonPrefix, snakeCase)
import           Database.Bolt              (Node (..), Relationship (..),
                                             URelationship (..))
import           Database.Bolt.Extras.Utils (currentLoc)
import           GHC.Generics               (Generic (..))

-- | 'BoltId' is alias for Bolt 'Node', 'Relationship' and 'URelationship' identities.
--
type BoltId = Int

-- | 'Persisted' is wrapper for some object that can be identified with 'BoltId'.
--
data Persisted a = Persisted { objectId    :: BoltId
                             , objectValue :: a
                             } deriving (Show, Functor, Eq, Ord, Read, Generic)

-- | This is just check that your 'BoltId' is valid.
--
fromInt :: Int -> BoltId
fromInt i | i >= 0    = i
          | otherwise = error $ $currentLoc ++ "could not create BoltId with identity less then zero."

-- Common class to get 'BoltId' from the object.
--
class GetBoltId a where
  getBoltId :: a -> BoltId

instance GetBoltId Node where
  getBoltId = fromInt . nodeIdentity

instance GetBoltId Relationship where
  getBoltId = fromInt . relIdentity

instance GetBoltId URelationship where
  getBoltId = fromInt . urelIdentity

instance GetBoltId (Persisted a) where
  getBoltId = fromInt . objectId

instance ToJSON a => ToJSON (Persisted a) where
  toJSON = genericToJSON $ aesonPrefix snakeCase

instance FromJSON a => FromJSON (Persisted a) where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase
