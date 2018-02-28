{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Database.Bolt.Extras.Template.Instances () where

import           Data.Map.Strict                     (Map)
import           Data.Text                           (Text)
import           Database.Bolt                       (Value (..))
import qualified Database.Bolt                       as DB (Structure)
import           Database.Bolt.Extras.Template.Types (FromValue (..),
                                                      ToValue (..))
import           Database.Bolt.Extras.Utils          (currentLoc)
import           GHC.Float                           (double2Float,
                                                      float2Double)

instance ToValue () where
  toValue = N

instance ToValue Bool where
  toValue = B

instance ToValue Int where
  toValue = I

instance ToValue Double where
  toValue = F

instance ToValue Float where
  toValue = F . float2Double

instance ToValue Text where
  toValue = T

instance ToValue Value where
  toValue = id

instance ToValue a => ToValue [a] where
  toValue = L . fmap toValue

instance ToValue a => ToValue (Maybe a) where
  toValue (Just a) = toValue a
  toValue _        = toValue ()

instance ToValue (Map Text Value) where
  toValue = M

instance ToValue DB.Structure where
  toValue = S

instance FromValue () where
  fromValue (N ()) = ()
  fromValue v      = error $ $currentLoc ++ "could not unpack " ++ show v ++ " into ()"

instance FromValue Bool where
  fromValue (B boolV) = boolV
  fromValue v      = error $ $currentLoc ++ "could not unpack " ++ show v ++ " into Bool"

instance FromValue Int where
  fromValue (I intV) = intV
  fromValue v      = error $ $currentLoc ++ "could not unpack " ++ show v ++ " into Int"

instance FromValue Double where
  fromValue (F doubleV) = doubleV
  fromValue v      = error $ $currentLoc ++ "could not unpack " ++ show v ++ " into Double"

instance FromValue Float where
  fromValue (F doubleV) = double2Float doubleV
  fromValue v      = error $ $currentLoc ++ "could not unpack " ++ show v ++ " into Float"

instance FromValue Text where
  fromValue (T textV) = textV
  fromValue v      = error $ $currentLoc ++ "could not unpack " ++ show v ++ " into Text"

instance FromValue Value where
  fromValue = id

instance FromValue a => FromValue [a] where
  fromValue (L listV) = fmap fromValue listV
  fromValue v      = error $ $currentLoc ++ "could not unpack " ++ show v ++ " into [Value]"

instance FromValue a => FromValue (Maybe a) where
  fromValue (N ()) = Nothing
  fromValue a = Just $ fromValue a

instance FromValue (Map Text Value) where
  fromValue (M mapV) = mapV
  fromValue v      = error $ $currentLoc ++ "could not unpack " ++ show v ++ " into (Map Text Value)"

instance FromValue DB.Structure where
  fromValue (S structureV) = structureV
  fromValue v      = error $ $currentLoc ++ "could not unpack " ++ show v ++ " into Structure"
