module Database.Bolt.Extras.Template
  ( FromValue (..)
  , Label
  , Labels (..)
  , Node (..)
  , NodeLike (..)
  , Properties (..)
  , Property
  , Relationship (..)
  , ToValue (..)
  , URelationLike (..)
  , URelationship (..)
  , Value (..)
  , makeNodeLike
  , makeURelationLike
  ) where

import           Database.Bolt.Extras.Template.Converters (makeNodeLike,
                                                           makeURelationLike)
import           Database.Bolt.Extras.Template.Instances  ()
import           Database.Bolt.Extras.Template.Types      (FromValue (..),
                                                           Label, Labels (..),
                                                           Node (..),
                                                           NodeLike (..),
                                                           Properties (..),
                                                           Property,
                                                           Relationship (..),
                                                           ToValue (..),
                                                           URelationLike (..),
                                                           URelationship (..),
                                                           Value (..))
