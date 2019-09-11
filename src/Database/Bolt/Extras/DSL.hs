module Database.Bolt.Extras.DSL
  (
    -- * Selectors for nodes, relations and paths
    --
    -- | These data types let you specify Cypher queries.
    --
    -- With @OverloadedLabels@ and operators you can write selectors in very concise
    -- Cypher-like form:
    --
    -- > (#n .: "Name" .# ["name" =: "C42"]) -: (defR .: "NAME_OF") :!->: (#m .: "Molecule")
    -- > (n:Name{name:"C42"})-[:NAME_OF]->(m:Molecule)
    --
    NodeSelector(..),
    RelSelector(..),
    SelectorLike(..),
    (.:), (.#),
    toNodeSelector, toRelSelector,
    PathSelector(..),
    PathPart(..),
    (-:), (<-:),
    Selector(..),
    Selectors,

    -- ** Default selectors
    defaultNode, defN, defaultRel, defR,

    -- * Cypher conditions
    Cond(..),
    Conds(..),

    -- * DSL for Cypher
    --
    -- | The free-monadic DSL lets you write Cypher queries in Haskell like this:
    --
    -- > formQuery $ do
    -- >    matchF [
    -- >      PS $ (#n .: "Name" .# ["name" =: "C42"]) -: (defR .: "NAME_OF") :!->: (#m .: "Molecule")
    -- >    ]
    -- >    returnF ["n", "m"]
    --

    -- ** DSL operations
    module Database.Bolt.Extras.DSL.Internal.Language,

    -- ** Rendering Cypher queries
    formQuery,

    -- ** Implementation details
    Expr(..)
  ) where

import           Database.Bolt.Extras.DSL.Internal.Executer
import           Database.Bolt.Extras.DSL.Internal.Instances ()
import           Database.Bolt.Extras.DSL.Internal.Language
import           Database.Bolt.Extras.DSL.Internal.Types
