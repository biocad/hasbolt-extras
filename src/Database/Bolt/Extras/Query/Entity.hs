{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Database.Bolt.Extras.Query.Entity where

import           Control.Monad              (zipWithM)
import           Data.Map.Strict            ((!))
import           Data.Text                  as T (Text, pack)
import           Database.Bolt              (BoltActionT, Node (..), Record,
                                             Relationship (..),
                                             URelationship (..), exact)
import           Database.Bolt.Extras.Utils
import           Text.Printf                (printf)

data Entity = NodeEntity Node | URelEntity URelationship

data EntityVar = NodeVar Text | URelVar Text

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



