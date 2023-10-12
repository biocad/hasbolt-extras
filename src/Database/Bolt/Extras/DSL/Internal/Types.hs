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
  , (.:)
  , (.#)
  , (#)
  , (-:)
  , (<-:)
  , defaultNode
  , defN
  , defaultRel
  , defR
  , toNodeSelector
  , toRelSelector
  ) where

import Data.Foldable        (foldl')
import Data.Map.Strict      (toList)
import Data.Text            (Text)
import Database.Bolt        (Node (..), URelationship (..), Value (..))
import Database.Bolt.Extras (BoltId)
import GHC.Stack            (HasCallStack)

-- | Class for Selectors, which can update identifier, labels and props.
--
class SelectorLike a where
   withIdentifier :: Text -> a -> a
   withLabel      :: Text -> a -> a
   withProp       :: (Text, Value) -> a -> a
   withParam      :: (Text, Text) -> a -> a

-- | Selector for 'Node's.
--
-- This datatype has @OverloadedLabels@ instance to simplify specifying nodes. Labels produce
-- empty nodes.
--
-- > #foo :: NodeSelector
-- > -- foo = NodeSelector (Just "foo") [] []
--
data NodeSelector = NodeSelector { nodeIdentifier :: Maybe Text
                                 , nodeLabels     :: [Text]
                                 , nodeProperties :: [(Text, Value)]
                                 , nodeParams     :: [(Text, Text)]
                                 }
  deriving (Show, Eq)

-- | Selector for 'URelationship's.
--
-- This datatype has @OverloadedLabels@ instance as well, similar to 'NodeSelector'.
data RelSelector = RelSelector { relIdentifier :: Maybe Text
                               , relLabel      :: Text
                               , relProperties :: [(Text, Value)]
                               , relParams     :: [(Text, Text)]
                               }
  deriving (Show, Eq)

-- | Operator version of 'withLabel'. To be used with @OverloadedLabels@ instances.
--
-- > #foo .: "Foo" :: NodeSelector
--
infixl 9 .:
(.:) :: SelectorLike a => a -> Text -> a
(.:) = flip withLabel

-- | Operator version of 'withProp'. To be used with @OverloadedLabels@ instances.
--
-- See also 'Database.Bolt.=:' from @Database.Bolt@ package.
--
-- > #foo .# ["bar" =: 42, "baz" =: "baz"] :: NodeSelector
--
infixl 9 .#
(.#) :: SelectorLike a => a -> [(Text, Value)] -> a
(.#) = foldl' (flip withProp)

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

-- | Combined version of ':-!:' and 'P' for specifying the first node of path.
--
infixl 1 -:
(-:) :: NodeSelector -> PathPart -> PathSelector
ns -: pp = P ns :-!: pp

-- | Combined version of ':<-!:' and 'P' for specifying the first node of path.
--
infixl 1 <-:
(<-:) :: NodeSelector -> PathPart -> PathSelector
ns <-: pp = P ns :<-!: pp

data Selector = PS PathSelector -- ^ path selector
              | PSwN (Text, Selector) -- ^ named selector
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
-- | Conditional expressions in Cypher.
--
-- Can be used with @OverloadedStrings@ to include atomic text conditions.
--
-- > toCypher $ ("x = 5" :&&: "y = 10") :||: Not "z = 20"
-- > "((x = 5) AND (y = 10)) OR (NOT (z = 20))"
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
  deriving (Show, Eq, Functor)

-- | Empty 'NodeSelector'.
defaultNode :: NodeSelector
defaultNode = NodeSelector Nothing [] [] []

-- | Shorter synonym for 'defaultRel'.
defN :: NodeSelector
defN = defaultNode

-- | Empty 'RelSelector'.
defaultRel :: RelSelector
defaultRel = RelSelector Nothing "" [] []

-- | Shorter synonym for 'defaultRel'.
defR :: RelSelector
defR = defaultRel

toNodeSelector :: HasCallStack => Node -> NodeSelector
toNodeSelector Node{..} = defaultNode { nodeLabels      = labels
                                      , nodeProperties  = filter ((/= N ()) . snd) (toList nodeProps)
                                      }

toRelSelector :: HasCallStack => URelationship -> RelSelector
toRelSelector URelationship{..} = defaultRel { relLabel      = urelType
                                             , relProperties = toList urelProps
                                             }
