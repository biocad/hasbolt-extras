{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Type Families that implement the logic of type-level labels.
module Database.Bolt.Extras.DSL.Typed.Families where

import           Data.Kind      (Constraint, Type)
import           Data.Type.Bool (If, type (||))
import           GHC.Generics   ((:*:), C1, D1, Meta (..), Rec0, Rep, S1)
import           GHC.TypeLits   (ErrorMessage (..), Symbol, TypeError)

-- | This family extracts name of the type from Generic 'Rep'.
type family GetTypeName (a :: k -> Type) :: Symbol where
  GetTypeName (D1 ('MetaData name _ _ _) _) = name

-- | This family checks whether a record (in a form of 'Rep') has a field with given name.
type family RecordHasField (field :: Symbol) (record :: k -> Type) :: Bool where
  RecordHasField field (D1 _ (C1 _ sels)) = RecordHasField field sels
  RecordHasField field (l :*: r) = RecordHasField field l || RecordHasField field r
  RecordHasField field (S1 ('MetaSel ('Just field) _ _ _) _) = 'True
  RecordHasField _ _ = 'False

-- | This family extracts the type of field with given name from Generic record in a 'Rep'.
type family GetTypeFromRecord (field :: Symbol) (record :: k -> Type) :: Type where
  GetTypeFromRecord field (D1 _ (C1 _ sels)) = GetTypeFromRecord field sels
  GetTypeFromRecord field (S1 ('MetaSel ('Just field) _ _ _) (Rec0 (Maybe typ))) = typ
  GetTypeFromRecord field (S1 ('MetaSel ('Just field) _ _ _) (Rec0 typ)) = typ
  GetTypeFromRecord field (S1 ('MetaSel ('Just field) _ _ _) (Rec0 (Maybe typ) ) :*: _) = typ
  GetTypeFromRecord field (S1 ('MetaSel ('Just field) _ _ _) (Rec0 typ ) :*: _) = typ
  GetTypeFromRecord field (S1 ('MetaSel ('Just _) _ _ _) (Rec0 typ ) :*: r) =
    GetTypeFromRecord field r

-- | This family extracts a type of the field with given name from the first type in the list
-- that has it.
type family GetTypeFromList (field :: Symbol) (types :: [Type]) :: Type where
  GetTypeFromList field (t ': ts)
    = If
        (RecordHasField field (Rep t))
        (GetTypeFromRecord field (Rep t))
        (GetTypeFromList field ts)

-- * Implementation of type errors

-- | Just a dummy type for implementation trick.
-- This is based on https://kcsongor.github.io/report-stuck-families/
data T1

type family Any :: k
-- | This family is able to check whether its argument is stuck and resolve with an error
-- in that case.
type family Assert (err :: Constraint) (a :: k) :: k where
  Assert _ T1 = Any
  Assert _ k = k

-- | A version of 'Assert' that returns trivial constraint @()@ when argument is not stuck,
-- discarding its actual value.
type family AssertC (err :: Constraint) (a :: k) :: Constraint where
  Assert _ T1 = Any
  Assert _ k = ()

-- | Error text for the case when records do no have the required field.
type family NoFieldError (field :: Symbol) (types :: [Type]) :: k where
  NoFieldError field types
    = TypeError
        ('Text "There is no field " ':<>: 'ShowType field ':<>: 'Text " in any of the records"
         ':$$: 'ShowType types
        )
