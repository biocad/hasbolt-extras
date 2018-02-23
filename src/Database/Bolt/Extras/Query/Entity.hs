{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Database.Bolt.Extras.Query.Entity where

import           Control.Monad              (zipWithM)
import           Data.Map.Strict            ((!))
import           Data.Text                  as T (Text, pack)
import           Database.Bolt              as Bolt (BoltActionT, Node (..),
                                                     Record, Relationship (..),
                                                     URelationship (..), exact)
import           Database.Bolt.Extras.Utils
import           Text.Printf                (printf)

-- | 'Entity' is wrapper for 'Node' or 'URelationship',
-- object that can be stored in database.
--
data Entity = NodeEntity Node | URelEntity URelationship

-- | 'EntityVar' is something like "typeable" variables for 'Node's and 'URelationship's.
--
data EntityVar = NodeVar Text | URelVar Text

-- | 'EntityLike' is what can be converted into and from 'Entity'.
--
class EntityLike a where
  toEntity :: a -> Entity
  fromEntity :: Entity -> a

instance EntityLike Node where
  toEntity = NodeEntity

  fromEntity (NodeEntity n) = n
  fromEntity _              = error $ $currentLoc ++ "could not unpack entity"

instance EntityLike URelationship where
  toEntity = URelEntity

  fromEntity (URelEntity r) = r
  fromEntity _              = error $ $currentLoc ++ "could not unpack entity"

-- | Generates variables for entities. For example, for given
-- sequence @[NodeEntity{}, URelEntity{}, NodeEntity{}]@ generates
-- @[NodeVar "N0", URelVar "R1", NodeVar "N2"]@.
--
generateEntityVars :: [Entity] -> [EntityVar]
generateEntityVars = zipWith (flip generator) [0..]
  where
    generator :: Entity -> Int -> EntityVar
    generator (NodeEntity _) = NodeVar . pack . printf "N%d"
    generator (URelEntity _) = URelVar . pack . printf "R%d"

-- | Exact 'Entity' by given variable name, which is used to
-- understand where to search 'Entity' in 'Record' Map and
-- what 'Entity' type is expected.
--
exactEntity :: EntityVar -> Record -> BoltActionT IO Entity
exactEntity (NodeVar var) record = NodeEntity <$> Bolt.exact (record ! var)
exactEntity (URelVar var) record = URelEntity . makeU <$> Bolt.exact (record ! var)
  where
    -- forget information about @startNodeId@ and @endNodeId@
    -- that are stored in 'Relationship'
    makeU :: Relationship -> URelationship
    makeU Relationship{..} = URelationship relIdentity relType relProps

-- | Exact sevelar 'Entity's by given 'EntityVar's and 'Record's.
exactEntities :: [EntityVar] -> [Record] -> BoltActionT IO [Entity]
exactEntities = zipWithM exactEntity



