{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Bolt.Extras.Generic where

import Data.Proxy    (Proxy (..))
import Data.Text     (pack, unpack)
import Database.Bolt (Value (..))
import GHC.Generics  (C1, D1, Generic (..), M1 (..), Meta (..), U1 (..), type (:+:) (..))
import GHC.TypeLits  (KnownSymbol, symbolVal)

import Control.Applicative                 ((<|>))
import Database.Bolt.Extras.Internal.Types (FromValue (..), ToValue (..))
import Type.Reflection                     (Typeable, typeRep)

-- | Wrapper to encode enum-like types as strings in the DB.
--
-- Intended usage is with @DerivingVia@:
--
-- >>> :{
-- data Color = Red | Green | Blue
--   deriving (Show, Generic)
--   deriving (ToValue, FromValue) via BoltEnum Color
-- :}
--
-- >>> toValue Red
-- T "Red"
-- >>> fromValue (T "Blue") :: Color
-- Blue
-- >>> fromValue (T "Brown") :: Color
-- *** Exception: Could not unpack unknown value Brown of Color
-- ...
-- ...
newtype BoltEnum a
  = BoltEnum a
  deriving (Eq, Show, Generic)

instance (Generic a, GToValue (Rep a)) => ToValue (BoltEnum a) where
  toValue (BoltEnum a) = T $ pack $ gToValue $ from a

instance (Typeable a, Generic a, GFromValue (Rep a)) => FromValue (BoltEnum a) where
  fromValue (T str) =
    case gFromValue $ unpack str of
      Nothing -> error $ "Could not unpack unknown value " <> unpack str <> " of " <> show (typeRep @a)
      Just rep -> BoltEnum $ to rep
  fromValue v = error $ "Could not unpack " <> show v <> " as " <> show (typeRep @a)

class GToValue rep where
  gToValue :: rep a -> String

instance GToValue cs => GToValue (D1 meta cs) where
  gToValue (M1 cs) = gToValue cs

instance KnownSymbol name => GToValue (C1 ('MetaCons name fixity rec) U1) where
  gToValue _ = symbolVal @name Proxy

instance (GToValue l, GToValue r) => GToValue (l :+: r) where
  gToValue (L1 l) = gToValue l
  gToValue (R1 r) = gToValue r

class GFromValue rep where
  gFromValue :: String -> Maybe (rep a)

instance GFromValue cs => GFromValue (D1 meta cs) where
  gFromValue = fmap M1 . gFromValue @cs

instance KnownSymbol name => GFromValue (C1 ('MetaCons name fixity rec) U1) where
  gFromValue str =
    if str == symbolVal @name Proxy
       then Just $ M1 U1
       else Nothing

instance (GFromValue l, GFromValue r) => GFromValue (l :+: r) where
  gFromValue str = L1 <$> gFromValue @l str <|> R1 <$> gFromValue @r str

{- $setup
>>> :set -XDerivingStrategies -XDerivingVia
>>> :load Database.Bolt.Extras Database.Bolt.Extras.Generic
>>> import GHC.Generics
>>> import Database.Bolt.Extras.Generic
>>> import Database.Bolt (Value (..))
-}
