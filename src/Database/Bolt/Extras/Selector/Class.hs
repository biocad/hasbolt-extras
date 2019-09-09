{-# LANGUAGE MultiParamTypeClasses #-}
module Database.Bolt.Extras.Selector.Class where

import Database.Bolt.Extras (Label, BoltId)
import Data.Text (Text)
import           Data.Foldable                  ( foldl' )
import           Database.Bolt                                     as B (Value)
import           Language.Haskell.TH.Syntax                        (Name)

-- | Endomorphisms to set up 'NodeGetter' and 'RelGetter'.
--
class GetterLike a where
    withBoltId :: BoltId          -> a -> a -- ^ set known 'BoltId'
    withLabel  :: Label           -> a -> a -- ^ set known label
    withLabelQ :: Name            -> a -> a -- ^ set known label as TemplateHaskell 'Name'
    withProp   :: (Text, B.Value) -> a -> a -- ^ add known property
    withReturn :: [Text]          -> a -> a -- ^ add list of properties to return
    isReturned ::                    a -> a -- ^ set that entity should be returned

infixl 9 .:
(.:) :: GetterLike a => (Text, a) -> Text -> (Text, a)
(n, g) .: l = (n, withLabel l g)

infixl 9 .#
(.#) :: GetterLike a => (Text, a) -> [(Text, Value)] -> (Text, a)
(n, g) .# ps = (n, foldl' (flip withProp) g ps)

-- | Entity which can be requested from Neo4j in @MATCH@ operator.
--
class Requestable a where
  -- | How to convert entity to Cypher.
  request         :: a -> Text

--infixl 9 -:
--infixl 9 ->:
--class PathExtendable a b c where
--  (-:-) :: a -> b -> c
--  (-:>) :: a -> b -> c

