module Database.Bolt.Extras.Query.Selectors
    ( NodeSelector (..)
    , RelSelector (..)
    , URelSelector (..)
    ) where

import qualified Data.Text        as T (Text)
import           Database.Bolt.Id (BoltId (..))

-- | Helper to find 'Node's.
-- _varQNName is the mark for this Node, which will be used in Cypher queries.
-- For example "MATCH(a)", here _varQNName = "a"
data NodeSelector = NodeSelector { boltIdQ :: Maybe BoltId
                                 , labelsS :: Maybe [T.Text]
                                 } deriving (Show)

-- | Helper to find 'Relationship's.
-- _varQRName is the mark for this Relationship, which will be used in Cypher queries.
-- For example "RETURN(a)", here _varQRName = "a".
-- RelSelector is used for searching using BoltId-s of 'Node's, connected by this Relationship.
data RelSelector = RelSelector { startNodeBoltId :: Maybe BoltId
                               , endNodeBoltId   :: Maybe BoltId
                               , typeS           :: Maybe T.Text
                               } deriving (Show)

-- | URelSelector is used for searching using indexes of 'Node's in the given graph.
data URelSelector = URelSelector { typeLS :: Maybe T.Text } deriving (Show)
