{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Bolt.Extras.DSL.Typed.Families where

import           Data.Kind          (Type)
import           Data.Type.Bool     (type (||))
import           Data.Type.Equality (type (==))
import           GHC.Generics       (C1, D1, Meta (..), Rec0, Rep, S1)
import           GHC.TypeLits       (Symbol)

type family GetTypeName (a :: k -> Type) :: Symbol where
  GetTypeName (D1 ('MetaData name _ _ _) _) = name

type family GetTypeFromRecord (field :: Symbol) (record :: k -> Type) :: Type where
  GetTypeFromRecord field (D1 _ (C1 _ sels)) = GetTypeFromRecord field sels
  GetTypeFromRecord field (S1 ('MetaSel ('Just field) _ _ _) (Rec0 typ)) = typ

type family IsType (field :: Symbol) (typ :: Type) (record :: Type) :: Bool where
  IsType field typ record = GetTypeFromRecord field (Rep record) == typ

type family AnyHasType (field :: Symbol) (typ :: Type) (types :: [Type]) :: Bool where
  AnyHasType field typ '[] = 'False
  AnyHasType field typ (t ': ts) = IsType field typ t || AnyHasType field typ ts
