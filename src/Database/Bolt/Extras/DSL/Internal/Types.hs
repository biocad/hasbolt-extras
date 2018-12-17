{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Database.Bolt.Extras.DSL.Internal.Types
  (
    NodeSelector (..)
  , RelSelector (..)
  , PathPart (..)
  , PathSelector (..)
  , Selector (..)
  , Selectors
  , Cond (..)
  , Conds (..)
  , Expr (..)
  , SelectorLike (..)
  , (#)
  , defaultNode
  , defaultRel
  , toNodeSelector
  , toRelSelector
  ) where

import           Data.Map.Strict      (toList)
import           Data.Text            (Text)
import           Database.Bolt        (Node (..), URelationship (..),
                                       Value (..))
import           Database.Bolt.Extras (BoltId)

-- | Class for Selectors, which can update identifier, labels and props.
--
class SelectorLike a where
   withIdentifier :: Text -> a -> a
   withLabel      :: Text -> a -> a
   withProp       :: (Text, Value) -> a -> a

-- | Selector for 'Node's.
--
data NodeSelector = NodeSelector { nodeIdentifier :: Maybe Text
                                 , nodeLabels     :: [Text]
                                 , nodeProperties :: [(Text, Value)]
                                 }
  deriving (Show, Eq)

-- | Selector for 'URelationship's.
--
data RelSelector = RelSelector { relIdentifier :: Maybe Text
                               , relLabel      :: Text
                               , relProperties :: [(Text, Value)]
                               }
  deriving (Show, Eq)


(#) :: a -> (a -> b) -> b
(#) = flip ($)

-- | Selector for paths.
--
infixl 2 :!->:
infixl 2 :!-:
data PathPart = RelSelector :!->: NodeSelector -- ^ directed relation
              | RelSelector :!-: NodeSelector  -- ^ not directed relation
  deriving (Show, Eq)

infixl 1 :-!:
infixl 1 :<-!:
data PathSelector = PathSelector :-!: PathPart  -- ^ not directed relation
                  | PathSelector :<-!: PathPart -- ^ directed relation
                  | P NodeSelector              -- ^ starting node of Path
  deriving (Show, Eq)

data Selector = PS PathSelector -- ^ path selector
              | TS Text         -- ^ free text selector
  deriving (Show, Eq)

type Selectors = [Selector]

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
               | Text Text next               -- ^ free text query
  deriving (Show, Eq, Functor)

defaultNode :: NodeSelector
defaultNode = NodeSelector Nothing [] []

defaultRel :: RelSelector
defaultRel = RelSelector Nothing "" []

toNodeSelector :: Node -> NodeSelector
toNodeSelector Node{..} = defaultNode { nodeLabels      = labels
                                      , nodeProperties  = filter ((/= N ()) . snd) (toList nodeProps)
                                      }

toRelSelector :: URelationship -> RelSelector
toRelSelector URelationship{..} = defaultRel { relLabel      = urelType
                                             , relProperties = toList urelProps
                                             }
