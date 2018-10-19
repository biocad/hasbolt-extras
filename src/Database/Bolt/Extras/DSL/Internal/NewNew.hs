module Database.Bolt.Extras.DSL.Internal.NewNew where

import           GHC.TypeLits
import           Data.Text
import           Data.Proxy

import           Data.Semigroup

import           Database.Bolt.Extras.DSL.Internal.Instances
                                                ( )
import qualified Database.Bolt.Extras.DSL.Internal.Types
                                               as Old
import           Database.Bolt.Extras.Query.Cypher

-- DataKinds promotes data constructors to type level
data Named = Named Symbol | Unnamed

-- KindSignatures extension lets us attach whatever terms to type level
data NodeSelector (name :: Named) = NodeSelector { nName :: Maybe Text, nLabels :: [Text] }
data RelationshipSelector (name :: Named) = RelationshipSelector { rName :: Maybe Text, rLabel :: Text }

-- Just ++ for type level lists
type family Union xs ys where
  Union '[] ys = ys
  Union (x:xs) ys = x ': Union xs ys

-- Append name to list if there is one
type family Add (xs :: [Symbol]) (y :: Named) where
  Add xs ('Named name) = name ': xs
  Add xs 'Unnamed = xs

-- Make a list of present names
type family Pair (x :: Named) (y :: Named) where
  Pair ('Named x) ('Named y) = '[x, y]
  Pair ('Named x) 'Unnamed = '[x]
  Pair 'Unnamed ('Named y) = '[y]
  Pair 'Unnamed 'Unnamed = '[]

infixl 2 :->
infixl 2 :-
-- This GADT propagates total list of variables from arguments
data PathPart (variables :: [Symbol]) where
  (:->) :: RelationshipSelector v -> NodeSelector n -> PathPart (Pair v n)
  (:-)  :: RelationshipSelector v -> NodeSelector n -> PathPart (Pair v n)

infixl 1 :-:
infixl 1 :<-:
data PathSelector (variables :: [Symbol]) where
  (:-:)  :: PathSelector v -> PathPart w -> PathSelector (Union v w)
  (:<-:) :: PathSelector v -> PathPart w -> PathSelector (Union v w)
  P :: NodeSelector v -> PathSelector (Add '[] v)

-- This checks that some name is present in the list
type family Elem (x :: Symbol) (xs :: [Symbol]) where
  Elem x (x:xs) = 'True
  Elem x (_:xs) = Elem x xs
  Elem x '[] = 'False

-- Constraint to use Elem more conveniently
type In x xs = Elem x xs ~ 'True

-- Final Tagless represntation of Cypher language. Actually FT is probably overkill here and
-- repr does not need any type arguments
class CypherSym (repr :: [Symbol] -> * -> *) where
  emptyC :: repr '[] ()
  matchC :: repr vs () -> PathSelector ws -> repr (Union vs ws) ()
  -- The type of returnC will let user return only nodes that are in vs
  -- Unfortunately I did not find a way to avoid passing explicit Proxy
  -- However, it should be possible
  returnC :: forall v vs. KnownSymbol v => In v vs => Proxy v -> repr vs () -> repr vs ()

-- Like they do it in squeal...
newtype CypherQuery (variables :: [Symbol]) a = UnsafeQuery { renderQuery :: Text }

-- Piggyback on existing ToCypher instances
nsToOld :: NodeSelector name -> Old.NodeSelector
nsToOld NodeSelector {..} = Old.NodeSelector
  { nodeIdentifier = nName
  , nodeLabels     = nLabels
  , nodeProperties = []
  }

rsToOld :: RelationshipSelector name -> Old.RelSelector
rsToOld RelationshipSelector {..} = Old.RelSelector
  { relIdentifier = rName
  , relLabel      = rLabel
  , relProperties = []
  }

ppToOld :: PathPart variables -> Old.PathPart
ppToOld (rs :-> ns) = rsToOld rs Old.:!->: nsToOld ns
ppToOld (rs :-  ns) = rsToOld rs Old.:!-: nsToOld ns

psToOld :: PathSelector variables -> Old.PathSelector
psToOld (ps :-:  pp) = psToOld ps Old.:-!: ppToOld pp
psToOld (ps :<-: pp) = psToOld ps Old.:-!: ppToOld pp
psToOld (P ns      ) = Old.P $ nsToOld ns

renderPS :: PathSelector v -> Text
renderPS = toCypher . psToOld

-- Interpreter for Final Tagless representation which just renders query as Cypher string
-- Obviously it must look differently when finished "for real:
instance CypherSym CypherQuery where
  emptyC = UnsafeQuery ""
  matchC (UnsafeQuery q) ps = UnsafeQuery $ q <> "MATCH " <> renderPS ps
  returnC p (UnsafeQuery q) = UnsafeQuery $ q <> " RETURN " <> pack (symbolVal p)

-- Constructor for named node that puts the same name into type and into value
namedNode :: forall name . KnownSymbol name => NodeSelector ('Named name)
namedNode =
  NodeSelector { nName = Just $ pack $ symbolVal $ Proxy @name, nLabels = [] }

{- Example usage
- node1 = namedNode @"node1"
- query = renderQuery $ emptyC # matchC (P node1) # returnC (Proxy @"node1") -- this works
- query = renderQuery $ emptyC # matchC (P node1) # returnC (Proxy @"node2") -- this results in compile error, since "node2" is unknown
-
- Ideally, instead of `namedNode @"node1"` it'd be better to use OverloadedLabels with just `#node1`
- Otherwise returnC should probably accept just NodeSelector instead of Symbol, since it makes no sense
- keeping two entities for the same thing: variable "node1" and Symbol "node1"
-
- In the "real" implementation interpeter will be able to produce most of Cypher features.
-}
