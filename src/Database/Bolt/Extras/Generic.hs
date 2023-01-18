{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Bolt.Extras.Generic where

import           Data.Map.Strict (lookup, singleton)
import           Data.Proxy      (Proxy (..))
import           Data.Text       (pack)
import           Database.Bolt   (IsValue (..), RecordValue (..),
                                  UnpackError (Not), Value (..))
import           GHC.Generics    (C1, D1, Generic (..), K1 (..), M1 (..),
                                  Meta (..), Rec0, S1, Selector (selName),
                                  U1 (..), type (:*:) (..), type (:+:) (..))
import           GHC.TypeLits    as GHC (ErrorMessage (Text), KnownSymbol,
                                         TypeError, symbolVal)

import           Data.Aeson      (Options, constructorTagModifier,
                                  defaultOptions, fieldLabelModifier)
import           Data.Either     (isRight)
import           Prelude         hiding (lookup)
import           Type.Reflection (Typeable)

-- | Wrapper to encode enum-like types as strings in the DB.
--
-- Intended usage is with @DerivingVia@:
--
-- >>> :{
-- data Color = Red | Green | Blue
--   deriving (Eq, Show, Generic)
--   deriving (IsValue, RecordValue) via BoltGeneric Color
-- data MyRec = MyRec
--   { field1 :: Int
--   , field2 :: [Text]
--   , field3 :: Double
--   , field4 :: Color
--   }
--   deriving (Eq, Show, Generic)
--   deriving (IsValue, RecordValue) via BoltGeneric MyRec
-- data MyHardRec = MyHard
--   { field1h :: Int
--   , field2h :: [Text]
--   , field3h :: MyRec
--   }
--   deriving (Eq, Show, Generic)
--   deriving (IsValue, RecordValue) via BoltGeneric MyHardRec
-- data FailTest = FailTest Int Int
--   deriving (Eq, Show, Generic)
--   deriving (IsValue, RecordValue) via BoltGeneric FailTest
-- :}
--
-- >>> Bolt.toValue Red
-- T "Red"
-- >>> let myRec = MyRec 1 [pack "hello"] 3.14 Red
-- >>> Bolt.toValue myRec
-- M (fromList [("field1",I 1),("field2",L [T "hello"]),("field3",F 3.14),("field4",T "Red")])
-- >>> let myHardRec = MyHard 2 [pack "Hello!"] myRec
-- >>> Bolt.toValue myHardRec
-- M (fromList [("field1h",I 2),("field2h",L [T "Hello!"]),("field3h",M (fromList [("field1",I 1),("field2",L [T "hello"]),("field3",F 3.14),("field4",T "Red")]))])
-- >>> (exactEither . Bolt.toValue) myHardRec == Right myHardRec
-- True
-- >>> Bolt.toValue $ FailTest 1 2
-- ^
-- <interactive>:643:1: error:
--     • Can't make IsValue for non-record, non-unit constructor
--     • In the expression: Bolt.toValue $ FailTest 1 2
--       In an equation for ‘it’: it = Bolt.toValue $ FailTest 1 2
{- $setup
>>> :set -XDerivingStrategies -XDerivingVia
>>> :load Database.Bolt.Extras Database.Bolt.Extras.Generic
>>> import GHC.Generics
>>> import Database.Bolt.Extras.Generic
>>> import Data.Text (Text, pack)
>>> import Database.Bolt as Bolt (Value (..), IsValue(toValue), RecordValue(exactEither))
-}

newtype BoltGeneric a
  = BoltGeneric a
  deriving (Eq, Show, Generic)

instance (Generic a, GIsValue (Rep a)) => IsValue (BoltGeneric a) where
  toValue (BoltGeneric a) =
    case gIsValue defaultOptions (from a) of
      Left err  -> error err
      Right res -> res

instance (Typeable a, Generic a, GRecordValue (Rep a)) => RecordValue (BoltGeneric a) where
  exactEither v = BoltGeneric . to <$> gExactEither v

class GIsValue rep where
  gIsValue :: Options -> rep a -> Either String Value

instance GIsValue cs => GIsValue (D1 meta cs) where
  gIsValue op (M1 cs) = gIsValue op cs

instance GIsValue cs => (GIsValue (C1 ('MetaCons s1 s2 'True) cs)) where
  gIsValue op (M1 cs) = gIsValue op cs

instance {-# OVERLAPPING #-} (KnownSymbol name) => GIsValue (C1 ('MetaCons name s2 'False) U1) where
  gIsValue op _ = Right $ T $ pack $ constructorTagModifier op $ symbolVal @name Proxy

instance TypeError ('GHC.Text "Can't make IsValue for non-record, non-unit constructor ")  => GIsValue (C1 ('MetaCons s1 s2 'False) cs) where
  gIsValue _ _ = error "not reachable"

instance (Selector s, IsValue a) => GIsValue (S1 s (Rec0 a)) where
  gIsValue op m@(M1 (K1 v)) = Right $ M $ singleton (pack name) (toValue v)
    where
      name = fieldLabelModifier op (selName m)

instance (GIsValue l, GIsValue r) => GIsValue (l :+: r) where
  gIsValue op (L1 l) = gIsValue op l
  gIsValue op (R1 r) = gIsValue op r

instance (GIsValue l, GIsValue r) => GIsValue (l :*: r) where
  gIsValue op (l :*: r) = do
    lRes <- gIsValue op l
    rRes <- gIsValue op r
    case (lRes, rRes) of
      (M ml, M mr) -> Right $ M $ ml <> mr
      _            -> Left "not record product type"

class GRecordValue rep where
  gExactEither :: Value -> Either UnpackError (rep a)

instance GRecordValue cs => GRecordValue (D1 meta cs) where
  gExactEither v = M1 <$> gExactEither v

instance GRecordValue cs => GRecordValue (C1 ('MetaCons s1 s2 'True) cs) where
  gExactEither v = M1 <$> gExactEither v

instance {-# OVERLAPPING #-} (KnownSymbol name) => GRecordValue (C1 ('MetaCons name s2 'False) U1) where
  gExactEither _ =  Right $ M1 U1

instance TypeError ('GHC.Text "Can't make GRecordValue for non-record, non-unit constructor ") => GRecordValue (C1 ('MetaCons s1 s2 'False) cs) where
  gExactEither _ = error "not reachable"

instance (KnownSymbol name, GRecordValue a) => GRecordValue (S1 ('MetaSel ('Just name) s1 s2 s3) a) where
  gExactEither (M m) =
    case lookup (pack name) m of
      Just v -> M1 <$> gExactEither v
      Nothing -> Left $ Not $ pack $ "selector with name:" ++ name ++ " not in record"
    where
      name = symbolVal @name Proxy
  gExactEither _ = Left $ Not "bad structure in selector case"

instance (GRecordValue l, GRecordValue r) => GRecordValue (l :*: r) where
  gExactEither v = (:*:) <$> gExactEither v <*> gExactEither v

instance (GRecordValue l, GRecordValue r) => GRecordValue (l :+: r) where
  gExactEither v =
    let res = L1 <$> gExactEither @l v in
    if isRight res then res else R1 <$> gExactEither @r v

instance (RecordValue a) => GRecordValue (K1 i a) where
  gExactEither v = K1 <$> exactEither v
