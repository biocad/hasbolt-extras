{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns         #-}


module Database.Bolt.Extras.Generic where

import           Data.Map.Strict (fromList, singleton, toList)
import           Data.Proxy      (Proxy (..))
import           Data.Text       (pack)
import           Database.Bolt   (IsValue (..), RecordValue (..), UnpackError (Not),
                                  Value (..))
import           GHC.Generics    (C1, Constructor, D1, Generic (..), K1 (..),
                                  M1 (..), Meta (..), S1, Selector (selName),
                                  U1 (..), type (:*:) (..), type (:+:) (..),
                                  conIsRecord, conName)
import           GHC.TypeLits    (KnownSymbol, symbolVal)

import           Data.Aeson      (Options,
                                  defaultOptions, fieldLabelModifier, constructorTagModifier)
import           Data.Either     (isRight)
import           Type.Reflection (Typeable)

-- | Wrapper to encode enum-like types as strings in the DB.
--
-- Intended usage is with @DerivingVia@:
--
-- >>> :{
-- data Color = Red | Green | Blue
--   deriving (Eq, Show, Generic)
--   deriving (IsValue, RecordValue) via BoltGeneric Color
--
-- data MyRec = MyRec
--   { field1 :: Int
--   , field2 :: [Text]
--   , field3 :: Double
--   , field4 :: Color
--   }
--   deriving (Eq, Show, Generic)
--   deriving (IsValue, RecordValue) via BoltGeneric MyRec
--
-- data MyHardRec = MyHard
--   { field1h :: Int
--   , field2h :: [Text]
--   , field3h :: MyRec
--   }
--   deriving (Eq, Show, Generic)
--   deriving (IsValue, RecordValue) via BoltGeneric MyHardRec
-- :}
-- 
-- >>> toValue Red
-- T "Red"
-- let myRec = MyRec 1 [pack "hello"] 3.14 Red
-- >>> toValue myRec
-- M (fromList [("field1",I 1),("field2",L [T "hello"]),("field3",F 3.14),("field4",T "Red")])
-- let myHardRec = MyHard 2 [pack "Hello!"] myRec
-- >>> toValue myHardRec
-- M (fromList [("field1h",I 2),("field2h",L [T "Hello!"]),("field3h",M (fromList [("field1",I 1),("field2",L [T "hello"]),("field3",F 3.14),("field4",T "Red")]))])
-- let res = exactEither @Value myHardRec
-- >>> res
-- Right (M (fromList [("field1h",I 2),("field2h",L [T "Hello!"]),("field3h",M (fromList [("field1",I 1),("field2",L [T "hello"]),("field3",F 3.14),("field4",T "Red")]))]))
-- >>> if res == Right myHardRec then "perfect!" else "very bad"
-- "perfect!"
-- ...
-- ...

newtype BoltGeneric a
  = BoltGeneric a
  deriving (Eq, Show, Generic)

instance (Generic a, GIsValue (Rep a)) => IsValue (BoltGeneric a) where
  toValue (BoltGeneric a) =
    case gIsValue defaultOptions (from a) of
      Left err -> error err
      Right res -> res

instance (Typeable a, Generic a, GRecordValue (Rep a)) => RecordValue (BoltGeneric a) where
  exactEither v = BoltGeneric . to <$> gExactEither id (Nothing, v)

class GIsValue rep where
  gIsValue :: Options -> rep a -> Either String Value

instance GIsValue cs => GIsValue (D1 meta cs) where
  gIsValue op (M1 cs) = gIsValue op cs

instance (Constructor c, GIsValue cs) => GIsValue (C1 c cs) where
  gIsValue op m@(M1 cs) =
    if conIsRecord m
        then gIsValue op cs
        else 
          case gIsValue op cs of
            Right (N ()) -> Right $ T $ pack $ constructorTagModifier op $ conName m
            Right _ -> Left "unsuitable constructor type"
            Left err -> Left err


instance (Selector s, GIsValue cs) => GIsValue (S1 s cs) where
  gIsValue op m@(M1 cs) =
    case gIsValue op cs of
      Right v -> Right $ M $ singleton (pack name) v
      v -> Left $ "selector " ++ name ++ "have strange field: " ++ show v
    where
      name = fieldLabelModifier op (selName m)

instance GIsValue U1 where
  gIsValue _ _ = Right $ N ()

instance (GIsValue l, GIsValue r) => GIsValue (l :+: r) where
  gIsValue op (L1 l) = gIsValue op l
  gIsValue op (R1 r) = gIsValue op r

instance (GIsValue l, GIsValue r) => GIsValue (l :*: r) where
  gIsValue op (l :*: r) = do
    lRes <- gIsValue op l
    rRes <- gIsValue op r
    case (lRes, rRes) of
      (M ml, M mr) -> Right $ M $ ml <> mr
      (L lList, L rList) -> Right $ L $ lList <> rList
      (L lList, v) ->  Right $ L $ v : lList
      (v, L rList) -> Right $ L $ v : rList
      (lv, rv) -> Right $ L [lv , rv]

instance (IsValue a) => GIsValue (K1 i a) where
  gIsValue _ (K1 a) = Right (toValue a)

class GRecordValue rep where
  gExactEither :: (String -> String) -> (Maybe String, Value) -> Either UnpackError (rep a)

instance GRecordValue cs => GRecordValue (D1 meta cs) where
  gExactEither modifyField v = M1 <$> gExactEither modifyField v

instance GRecordValue cs => GRecordValue (C1 c cs) where
  gExactEither modifyField v = M1 <$> gExactEither modifyField v

instance (GHC.TypeLits.KnownSymbol name, GRecordValue a) => GRecordValue (S1 ('MetaSel ('Just name) s1 s2 s3) a) where
  gExactEither modifyField (Just name', v) =
    if modifyField name' == GHC.TypeLits.symbolVal @name Proxy
      then M1 <$> gExactEither modifyField (Nothing, v)
      else Left $ Not "bad name for selector"
  gExactEither _ (Nothing, _) = Left $ Not "bad name for selector"

instance (GRecordValue l, GRecordValue r) => GRecordValue (l :*: r) where
  gExactEither modifyField (_, v) =
    case v of
      M m ->
        let (fromList -> fstL, fromList -> sndL) = splitAt (length m `div` 2) $ toList m in
        (:*:) <$> gExactEither modifyField (Nothing, M fstL) <*> gExactEither modifyField (Nothing, M sndL)
      L l ->
        let (fstL, sndL) = splitAt (length l `div` 2) l in
        (:*:) <$> gExactEither modifyField (Nothing, L fstL) <*> gExactEither modifyField (Nothing, L sndL)
      _ -> Left $ Not "bad structure"


instance (GRecordValue l, GRecordValue r) => GRecordValue (l :+: r) where
  gExactEither modifyField v =
    let res = L1 <$> gExactEither @l modifyField v in
    if isRight res
      then res
      else R1 <$> gExactEither @r modifyField v

instance (RecordValue a) => GRecordValue (K1 i a) where
  gExactEither _ (_, v) = K1 <$> exactEither v

instance GRecordValue U1 where
  gExactEither _ (_, v) = 
    if v == N () 
      then Right U1 
      else Left $ Not "fail with Constructor without arguments - it's not ()"
