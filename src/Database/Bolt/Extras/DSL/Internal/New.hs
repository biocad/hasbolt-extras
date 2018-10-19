module Database.Bolt.Extras.DSL.Internal.New where

import Data.Text
import GHC.TypeLits
import GHC.OverloadedLabels

data NodeName (name :: Symbol) = NodeName
instance IsLabel name (NodeName name) where
  fromLabel = NodeName

data AnyNode = AnyNode

data RelationshipName (name :: Symbol) = RelationshipName
instance IsLabel name (RelationshipName name) where
  fromLabel = RelationshipName

data AnyRelationship = AnyRelationship

data NodeSelector (variables :: [Symbol]) = NodeSelector
  { nodeIdentifier :: Maybe Text
  , nodeLabels :: [Text]
  }

data RelationshipSelector (variables :: [Symbol]) = RelationshipSelector
  { relIdentifier :: Maybe Text
  , relLabels :: [Text]
  }

class IsNode a where
  fromNode :: a -> NodeSelector vs

-- Probably this class can be avoided
instance KnownSymbol name => IsNode (NodeName name) where
  fromNode name = NodeSelector { nodeIdentifier = Just . pack . symbolVal $ name, nodeLabels = [] }

instance KnownSymbol name => IsLabel name (NodeSelector '[name]) where
  fromLabel = fromNode (NodeName @name)

(&) = flip ($)

class Selector (sel :: [Symbol] -> *) where
  withLabel :: Text -> sel variables -> sel variables

instance Selector NodeSelector where
  withLabel label s@NodeSelector{..} = s { nodeLabels = label : nodeLabels }

instance Selector RelationshipSelector where
  withLabel label s@RelationshipSelector{..} = s { relLabels = label : relLabels }

anyNode :: NodeSelector '[]
anyNode = NodeSelector { nodeIdentifier = Nothing, nodeLabels = [] }

sel1 :: NodeSelector '["testNode"]
sel1 = #testNode & withLabel "Project"

--sel2 :: NodeSelector '[]
sel2 = anyNode & withLabel "Project"

class IsRelationship a where
  fromRelationship :: a -> RelationshipSelector variables

instance KnownSymbol name => IsRelationship (RelationshipName name) where
  fromRelationship name = RelationshipSelector { relIdentifier = Just . pack . symbolVal $ name, relLabels = [] }

instance KnownSymbol name => IsLabel name (RelationshipSelector variables) where
  fromLabel = fromRelationship (RelationshipName @name)

sel3 :: RelationshipSelector variables
sel3 = #testRel & withLabel "INCLUDE_TARGET"

type family Union xs ys where
  Union '[] ys = ys
  Union (x:xs) ys = x ': Union xs ys

infixl 2 :->
infixl 2 :-
data PathPart (variables :: [Symbol]) where
  (:->) :: RelationshipSelector v -> NodeSelector w -> PathPart (Union v w)
  (:-)  :: RelationshipSelector v -> NodeSelector w -> PathPart (Union v w)

infixl 1 :-:
infixl 1 :<-:
data PathSelector (variables :: [Symbol]) where
  (:-:)  :: PathSelector v -> PathPart w -> PathSelector (Union v w)
  (:<-:) :: PathSelector v -> PathPart w -> PathSelector (Union v w)
  P :: NodeSelector v -> PathSelector v

-- Does not work: GHC can't infer correct type level symbols for that
-- I believe there is a way to fix IsLabel instance, but will do it later

--sel4 :: PathSelector '["node1", "someRel", "node2"]
--sel4 = P #node1 :-: #someRel :- #node2

