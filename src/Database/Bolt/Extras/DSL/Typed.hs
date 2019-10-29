{- | Type-safe DSL for Cypher

This module extends selectors from 'Database.Bolt.Extras.DSL.DSL' with extra type-level
information to make them more type-safe to use.

None of additional type information exists at runtime, so using this module does not degrade
performance at all.

-}

module Database.Bolt.Extras.DSL.Typed
  (

  -- * Selecting Nodes and Relations
  --
  -- $selecting

  -- ** Type safety
  --
  -- $safety

    SelectorLike(..)
  , lbl
  , prop
  , (=:)
  , NodeSelector, RelSelector
  , defN
  , defR

  -- * Building paths
  --
  -- $paths

  , (.&)
  , (!->:)
  , (!-:)
  , (-:)
  , (<-:)
  , p
  ) where


import Database.Bolt.Extras.DSL.Typed.Types
import Database.Bolt.Extras.DSL.Typed.Instances ()

{- $setup
>>> :set -XDeriveGeneric
>>> :set -XTypeApplications
>>> :set -XOverloadedLabels
>>> :set -XOverloadedStrings
>>> import Data.Text (Text, unpack)
>>> import GHC.Generics (Generic)
>>> import Database.Bolt.Extras (toCypher)
>>> toCypherN = putStrLn . unpack . toCypher  . unsafeNodeSelector
>>> toCypherR = putStrLn . unpack . toCypher . unsafeRelSelector
>>> toCypherP = putStrLn . unpack . toCypher
>>> data Binder = Binder { uuid :: Text } deriving (Generic)
>>> data Foo = Foo { foo :: Int } deriving (Generic)
>>> data PLACE = PLACE deriving (Generic)
>>> data ELEMENT = ELEMENT deriving (Generic)
>>> data Name = Name { name :: Text } deriving (Generic)
>>> data User = User { user :: Text } deriving (Generic)
>>> data NAME_OF = NAME_OF deriving (Generic)
>>> data USER_CREATED = USER_CREATED { timestamp :: Int } deriving (Generic)
>>> data Library = Library deriving (Generic)
>>> data BinderLibrary = BinderLibrary deriving (Generic)
>>> import Database.Bolt.Extras.DSL (createF, mergeF, Selector(..), formQuery, returnF)
>>> toCypherQ = putStrLn . unpack . formQuery
-}

{- $selecting

There are types for Node and Relationship selectors: 'NodeSelector' and 'RelSelector'.
Both of them carry extra type-level information about labels assigned to Cypher variables.

Empty selectors may be constructed with 'defN' and 'defR' respectively. Selectors can be
extended with the following combinators:

- 'withIdentifier' adds an identifier (variable name)
- 'lbl' adds a label represented by some Haskell type
- 'prop' adds a new property, making sure that this property exists in one of the labels and
  has correct type

Typically selectors are chained by '.&' starting from 'defN' or 'defR' like this:

>>> toCypherN $ defN .& withIdentifier "binder" .& lbl @Binder .& prop (#uuid =: "123-456")
(binder:Binder{uuid:"123-456"})

Alternatively, @OverloadedLabels@ may be used to create an empty selector with an identifier:

>>> toCypherN $ #binder .& lbl @Binder .& prop (#uuid =: "123-456")
(binder:Binder{uuid:"123-456"})

This syntax is more concise and makes it obvious what is going on. Thus, it is the preferred
one.

The type used with 'lbl' should have 'GHC.Generics.Generic' instance.

Nodes may have multiple labels:

>>> toCypherN $ defN .& lbl @Binder .& lbl @Foo
(:Foo:Binder)

But relations have at most one:

>>> defR .& lbl @PLACE .& lbl @ELEMENT
...
... Can't add a new label to relationship selector that already has label PLACE!
...

==== Complex queries

These selectors are fully compatible with the 'Database.Bolt.Extras.DSL.DSL':

>>> :{
toCypherQ $ do
   mergeF
     [ PS $ p $ #name .& lbl @Name .& prop (#name =: "CT42")
     ]
   mergeF
     [ PS $ p $ #user .& lbl @User .& prop (#user =: "123-456")
     ]
   createF
     [ PS $ p $ #lib .& lbl @Library .& lbl @BinderLibrary
     , PS $ #name -: defR .& lbl @NAME_OF !->: #lib
     , PS $ #user -: defR .& lbl @USER_CREATED .& prop (#timestamp =: 1572340394000) !->: #lib
     ]
   returnF ["lib"]
:}
MERGE (name:Name{name:"CT42"}) MERGE (user:User{user:"123-456"}) CREATE (lib:BinderLibrary:Library), (name)-[:NAME_OF]->(lib), (user)-[:USER_CREATED{timestamp:1572340394000}]->(lib) RETURN lib

-}

{- $safety

Obviosuly, if you try to use @lbl \@Foo@ syntax with undefined type @Foo@, GHC itself
will report the error.

Here are more interesting cases:

>>> -- Properties are looked for in all labels
>>> toCypherN $ defN .& lbl @Binder .& lbl @Foo .& prop (#foo =: 42) .& prop (#uuid =: "123-456")
(:Foo:Binder{uuid:"123-456",foo:42})

>>> -- Adding a property to node without any labels
>>> defN .& prop (#uuid =: "123-456")
...
... There is no field "uuid" in any of the records
... '[]
...

>>> -- Adding a property that does not exist in the label
>>> defN .& lbl @Binder .& prop (#foo =: 42)
...
... There is no field "foo" in any of the records
... '[Binder]
...

>>> -- Adding a property with wrong type
>>> defN .& lbl @Binder .& prop (#uuid =: 42)
...
... No instance for (Num Text) arising from the literal ‘42’
...

Here we see that GHC undestands that the property should have type @Text@ and tries to unify it with
the type of literal @42@, which is @Num a => a@.

>>> -- Adding a property to relationship without a label
>>> defR .& prop (#foo =: 42)
...
... Tried to set property "foo" on a relationship without label!
...
-}

{- $paths

This module is completely interopable with path selectors from 'Database.Bolt.Extras.DSL.DSL' —
adding a 'NodeSelector' or 'RelSelector' to path simply drops all type information, converting it
into untyped variant.

Due to limitation of what symbols are allowed in operators and operator-like data constructors, this
module renames some of the path constructors. Precedence of the operators allow them to be combined
in the same expression with '.&' and '$' without any extra parentheses.

Here is an example of a path constructed this way:

>>> toCypherP (#binder .& lbl @Binder .& prop (#uuid =: "123") -: defR .& lbl @ELEMENT !->: #el)
(binder:Binder{uuid:"123"})-[:ELEMENT]->(el)
-}
