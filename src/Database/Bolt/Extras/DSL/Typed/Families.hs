{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Bolt.Extras.DSL.Typed.Families where

import           Data.Kind      (Constraint, Type)
import           Data.Type.Bool (If)
import           GHC.Generics   (C1, D1, Meta (..), Rec0, Rep, S1)
import           GHC.TypeLits   (ErrorMessage (..), Symbol, TypeError)

type family GetTypeName (a :: k -> Type) :: Symbol where
  GetTypeName (D1 ('MetaData name _ _ _) _) = name

type family RecordHasField (field :: Symbol) (record :: k -> Type) :: Bool where
  RecordHasField field (D1 _ (C1 _ sels)) = RecordHasField field sels
  RecordHasField field (S1 ('MetaSel ('Just field) _ _ _) _) = 'True
  RecordHasField _ _ = 'False

type family GetTypeFromRecord (field :: Symbol) (record :: k -> Type) :: Type where
  GetTypeFromRecord field (D1 _ (C1 _ sels)) = GetTypeFromRecord field sels
  GetTypeFromRecord field (S1 ('MetaSel ('Just field) _ _ _) (Rec0 typ)) = typ

type family GetTypeFromList (field :: Symbol) (types :: [Type]) :: Type where
  GetTypeFromList field (t ': ts)
    = If
        (RecordHasField field (Rep t))
        (GetTypeFromRecord field (Rep t))
        (GetTypeFromList field ts)

data T1

type family Any :: k
type family Assert (err :: Constraint) (a :: k) :: k where
  Assert _ T1 = Any
  Assert _ k = k

type family NoFieldError (field :: Symbol) (types :: [Type]) :: k where
  NoFieldError field types
    = TypeError
        ('Text "There is no field " ':<>: 'ShowType field ':<>: 'Text " in any of the records"
         ':$$: 'ShowType types
        )
