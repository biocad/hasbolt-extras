{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Database.Bolt.Extras.DSL.Typed.Instances where

import           Data.Coerce                             (coerce)
import           Data.Function                           ((&))
import           Data.Kind                               (Type)
import           Data.Text                               (pack)
import           GHC.Exts                                (proxy#)
import           GHC.Generics                            (Rep)
import           GHC.OverloadedLabels                    (IsLabel (..))
import           GHC.TypeLits                            (KnownSymbol, Symbol,
                                                          symbolVal')

import qualified Database.Bolt                           as B
import qualified Database.Bolt.Extras.DSL                as UT

import           Database.Bolt.Extras.DSL.Typed.Families
import           Database.Bolt.Extras.DSL.Typed.Types

instance (KnownSymbol x, types ~ '[]) => IsLabel x (NodeSelector types) where
  fromLabel = defN & withIdentifier (pack $ symbolVal' @x proxy#)

instance (KnownSymbol x, types ~ 'Nothing) => IsLabel x (RelSelector types) where
  fromLabel = defR & withIdentifier (pack $ symbolVal' @x proxy#)

instance (field ~ field1, KnownSymbol field) => IsLabel field (SymbolS field1) where
  fromLabel = SymbolS $ symbolVal' @field proxy#

instance SelectorLike NodeSelector where
  type CanAddType _ = ()
  type AddType (types :: [Type]) (typ :: Type) = typ ': types
  type HasField (types :: [Type]) (field :: Symbol) (typ :: Type) = AnyHasType field typ types ~ 'True

  withIdentifier = coerce $ UT.withIdentifier @UT.NodeSelector
  withLabel
    :: forall (typ :: Type) (types :: [Type]) (label :: Symbol)
    .  label ~ GetTypeName (Rep typ)
    => KnownSymbol label
    => NodeSelector types -> NodeSelector (typ ': types)
  withLabel = coerce $ UT.withLabel @UT.NodeSelector $ pack $ symbolVal' @label proxy#

  withProp
    :: forall (field :: Symbol) (types :: [Type]) (typ :: Type)
    .  B.IsValue typ
    => (SymbolS field, typ) -> NodeSelector types -> NodeSelector types
  withProp (SymbolS field, val) = coerce $ UT.withProp @UT.NodeSelector $ pack field B.=: val

instance SelectorLike RelSelector where
  type CanAddType 'Nothing = ()
  type CanAddType ('Just a) = 'False ~ 'True
  type AddType 'Nothing (typ :: Type) = 'Just typ
  type HasField ('Just record) (field :: Symbol) (typ :: Type) = IsType field typ record ~ 'True

  withIdentifier = coerce $ UT.withIdentifier @UT.RelSelector
  withLabel
    :: forall (typ :: Type) (types :: Maybe Type) (label :: Symbol)
    .  CanAddType types
    => GetTypeName (Rep typ) ~ label
    => KnownSymbol label
    => RelSelector types -> RelSelector (AddType types typ)
  withLabel = coerce $ UT.withLabel @UT.RelSelector $ pack $ symbolVal' @label proxy#

  withProp
    :: forall (field :: Symbol) (types :: Maybe Type) (typ :: Type)
    .  B.IsValue typ
    => (SymbolS field, typ) -> RelSelector types -> RelSelector types
  withProp (SymbolS field, val) = coerce $ UT.withProp @UT.RelSelector $ pack field B.=: val
