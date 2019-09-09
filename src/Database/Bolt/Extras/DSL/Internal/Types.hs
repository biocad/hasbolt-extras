{-# LANGUAGE DeriveFunctor #-}

module Database.Bolt.Extras.DSL.Internal.Types
  (
    Cond (..)
  , Conds (..)
  , Expr (..)
  ) where

import           Data.Text                     (Text)
import           Database.Bolt.Extras          (BoltId)
import           Database.Bolt.Extras.Selector (Selectors)

-- | Conditions.
--
data Cond = ID Text BoltId    -- ^ ID(txt) = boltId
          | IDs Text [BoltId] -- ^ ID(txt) IN [boltId1, boltId2, ... ]
          | IN Text [Text]    -- ^ txt IN [txt1, txt2, ... ]
          | TC Text           -- ^ free text condition
  deriving (Show, Eq)

infixr 3 :&&:
infixr 2 :||:
data Conds = Conds :&&: Conds -- ^ 'condition' AND 'condition'
           | Conds :||: Conds -- ^ 'condition' OR 'condition'
           | C Cond           -- ^ single 'condition'
           | Not Conds        -- ^ NOT 'condition'
  deriving (Show, Eq)

-- | Expression in Cypher language.
--
data Expr next = Create Selectors next        -- ^ CREATE query
               | Match Selectors next         -- ^ MATCH query
               | OptionalMatch Selectors next -- ^ OPTIONAL MATCH query
               | Merge Selectors next         -- ^ MERGE query
               | Where Conds next             -- ^ WHERE query
               | Set [Text] next              -- ^ SET query
               | Delete [Text] next           -- ^ DELETE query
               | DetachDelete [Text] next     -- ^ DETACH DELETE query
               | Remove [Text] next           -- ^ REMOVE query
               | Return [Text] next           -- ^ RETURN query
               | With [Text] next             -- ^ WITH query
               | Text Text next               -- ^ free text query
  deriving (Functor)
