{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Database.Bolt.Extras.DSL.Typed.Types where

import           Data.Kind                               (Constraint, Type)
import           Data.Text                               (Text)
import qualified Database.Bolt                           as B
import           GHC.Generics                            (Rep)
import           GHC.TypeLits                            (KnownSymbol, Symbol)

import qualified Database.Bolt.Extras.DSL                as UT

import           Database.Bolt.Extras.DSL.Typed.Families

class SelectorLike (a :: k -> Type) where
  type CanAddType (types :: k) :: Constraint
  type AddType (types :: k) (typ :: Type) = (result :: k) | result -> types typ
  type HasField (types :: k) (field :: Symbol) (typ :: Type) :: Constraint

  withIdentifier :: Text -> a types -> a types
  withLabel
    :: CanAddType types
    => KnownSymbol (GetTypeName (Rep typ))
    => a types
    -> a (AddType types typ)
  withProp
    :: HasField types field typ
    => B.IsValue typ
    => (SymbolS field, typ)
    -> a types
    -> a types

lbl
  :: forall (typ :: Type) k (types :: k) (a :: k -> Type)
  .  SelectorLike a
  => CanAddType types
  => KnownSymbol (GetTypeName (Rep typ))
  => a types
  -> a (AddType types typ)
lbl = withLabel

newtype NodeSelector (typ :: [Type])
  = NodeSelector { unsafeNodeSelector :: UT.NodeSelector }
    deriving (Show, Eq)

newtype RelSelector (typ :: Maybe Type)
  = RelSelector { unsafeRelSelector :: UT.RelSelector }
    deriving (Show, Eq)

newtype SymbolS (s :: Symbol) = SymbolS { getSymbol :: String }
  deriving (Show)

infixl 2 =:
(=:) :: forall (field :: Symbol) (typ :: Type). SymbolS field -> typ -> (SymbolS field, typ)
(=:) = (,)

infixl 1 .#
(.#)
  :: forall (field :: Symbol) (a :: k -> Type) (types :: k) (typ :: Type)
  .  SelectorLike a
  => HasField types field typ
  => B.IsValue typ
  => a types -> (SymbolS field, typ) -> a types
(.#) = flip withProp

defN :: NodeSelector '[]
defN = NodeSelector UT.defaultNode

defR :: RelSelector 'Nothing
defR = RelSelector UT.defaultRel
