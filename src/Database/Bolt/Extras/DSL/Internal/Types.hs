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
  , Cond
  , Expr (..)
  , SelectorLike (..)
  , (#)
  , defaultNode
  , defaultRel
  , toNodeSelector
  , toRelSelector
  ) where

import           Data.Map.Strict (toList)
import           Data.Text       (Text)
import           Database.Bolt   (Node (..), URelationship (..), Value (..))

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
data PathPart = RelSelector :!->: NodeSelector
              | RelSelector :!-: NodeSelector
  deriving (Show, Eq)

infixl 1 :-!:
infixl 1 :<-!:
data PathSelector = PathSelector :-!: PathPart
                  | PathSelector :<-!: PathPart
                  | P NodeSelector
  deriving (Show, Eq)

data Selector = PS PathSelector | TS Text
  deriving (Show, Eq)

type Selectors = [Selector]

type Cond = [Text]

-- | Expression in Cypher language.
--
data Expr next = Create Selectors next
               | Match Selectors next
               | OptionalMatch Selectors next
               | Merge Selectors next
               | Where Cond next
               | Set [Text] next
               | Delete [Text] next
               | DetachDelete [Text] next
               | Return [Text] next
               | Text Text next
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
