{-# LANGUAGE TemplateHaskell #-}

module Database.Bolt.Id
  ( BoltId (boltId)
  , GetBoltId (..)
  , fromInt
  ) where

import           Database.Bolt              (Node (..), Relationship (..),
                                             URelationship (..))
import           Database.Bolt.Extras.Utils (currentLoc)

-- | 'BoltId' is wrapper for Bolt 'Node', 'Relationship' and 'URelationship' identities.
newtype BoltId = BoltId { boltId :: Int }
  deriving (Show, Read, Eq, Ord)

fromInt :: Int -> BoltId
fromInt i | i >= 0    = BoltId i
          | otherwise = error $ $currentLoc ++ "could not create BoltId with identity less then zero."

class GetBoltId a where
  getBoltId :: a -> BoltId

instance GetBoltId Node where
  getBoltId = fromInt . nodeIdentity

instance GetBoltId Relationship where
  getBoltId = fromInt . relIdentity

instance GetBoltId URelationship where
  getBoltId = fromInt . urelIdentity
