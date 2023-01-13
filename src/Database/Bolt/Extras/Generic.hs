{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE InstanceSigs         #-}
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
import           Database.Bolt   (IsValue (..), RecordValue (..), UnpackError,
                                  Value (..))
import           GHC.Generics    (C1, Constructor, D1, Generic (..), K1 (..),
                                  M1 (..), Meta (..), S1, Selector (selName),
                                  U1 (..), conIsRecord, conName,
                                  type (:*:) (..), type (:+:) (..))
import           GHC.TypeLits    (KnownSymbol, symbolVal)

import           Data.Aeson      (Options, constructorTagModifier,
                                  defaultOptions, fieldLabelModifier)
import           Data.Either     (isLeft)
import           Type.Reflection (Typeable)

-- | Wrapper to encode enum-like types as strings in the DB.
--
-- Intended usage is with @DerivingVia@:
--

-- data Color = Red | Green | Blue
--   deriving (Eq, Show, Generic)
--   deriving (IsValue, RecordValue) via BoltGeneral Color

-- data MyRec = MyRec
--   { field1 :: Int
--   , field2 :: [Text]
--   , field3 :: Double
--   , field4 :: Color
--   }
--   deriving (Eq, Show, Generic)
--   deriving (IsValue, RecordValue) via BoltGeneral MyRec

-- data MyHardRec = MyHard
--   { field1h :: Int
--   , field2h :: [Text]
--   , field3h :: MyRec
--   }
--   deriving (Eq, Show, Generic)
--   deriving (IsValue, RecordValue) via BoltGeneral MyHardRec

-- test :: IO ()
-- test = do
--   print $ toValue Red
--   let myRec = MyRec 1 [pack "hello"] 3.14 Red
--   print $ toValue myRec
--   let myHardRec = toValue $ MyHard 2 [pack "Hello!"] myRec
--   print myHardRec
--   let res = exactEither @Value myHardRec
--   print res
--   print $ if res == Right myHardRec then "perfect!" else "very bad"


newtype BoltGeneral a
  = BoltGeneral a
  deriving (Eq, Show, Generic)

instance (Generic a, GIsValue (Rep a)) => IsValue (BoltGeneral a) where
  toValue (BoltGeneral a) = convert $ gIsValue defaultOptions $ from a
    where
      convert (Tuple _ _)     = error "selector without constructor"
      convert (ListOfValue l) = L $ map convert l
      convert (V v)           = v

instance (Typeable a, Generic a, GRecordValue (Rep a)) => RecordValue (BoltGeneral a) where
  exactEither v = BoltGeneral . to <$> gExactEither id (Nothing, v)

data ValueWithTuple
  = Tuple String Value
  | ListOfValue [ValueWithTuple]
  | V Value
  deriving (Show)

class GIsValue rep where
  gIsValue :: Options -> rep a -> ValueWithTuple

instance GIsValue cs => GIsValue (D1 meta cs) where
  gIsValue op (M1 cs) = gIsValue op cs

instance (Constructor c, GIsValue cs) => GIsValue (C1 c cs) where
  gIsValue :: Constructor c => Options -> C1 c cs a -> ValueWithTuple
  gIsValue op m@(M1 cs) =
    if conIsRecord m
      then
        case gIsValue op cs of
          Tuple name v -> V $ M $ singleton (pack name) v
          ListOfValue list ->
            if checkList list
              then V $ M $ fromList $ map (\(Tuple name v) -> (pack name, v)) list
              else error $ "in record" ++ conName m ++ "some value without name"
          V v -> error $ "in record" ++ conName m ++ "value: " ++ show v ++ "without name"
      else
        V $ T $ pack $ constructorTagModifier op $ conName m
    where
      checkList []               = True
      checkList (Tuple _ _ : ls) = checkList ls
      checkList _                = False

instance (Selector s, GIsValue cs) => GIsValue (S1 s cs) where
  gIsValue op m@(M1 cs) =
    case gIsValue op cs of
      V v -> Tuple (selName m) v
      v -> error $ "selector " ++ fieldLabelModifier op (selName m) ++ "have strange field: " ++ show v

instance GIsValue U1 where
  gIsValue _ _ = V $ N ()

instance (GIsValue l, GIsValue r) => GIsValue (l :+: r) where
  gIsValue :: (GIsValue l, GIsValue r) => Options -> (:+:) l r a -> ValueWithTuple
  gIsValue op (L1 l) = gIsValue op l
  gIsValue op (R1 r) = gIsValue op r

instance (GIsValue l, GIsValue r) => GIsValue (l :*: r) where
  gIsValue :: (GIsValue l, GIsValue r) => Options -> (:*:) l r a -> ValueWithTuple
  gIsValue op (l :*: r) =
    case (gIsValue op l, gIsValue op r) of
      (ListOfValue lList, ListOfValue rList) -> ListOfValue $ lList ++ rList
      (ListOfValue lList, t@(Tuple _ _)) -> ListOfValue $ t : lList
      (t@(Tuple _ _), ListOfValue rList) -> ListOfValue $ t : rList
      (t1@(Tuple _ _), t2@(Tuple _ _)) -> ListOfValue [t1, t2]
      (V val, _) -> error $ "in selector case value without name: " ++ show val
      (_ , V val) -> error $ "in selector case value without name: " ++ show val

instance (IsValue a) => GIsValue (K1 i a) where
  gIsValue _ (K1 a) = V (toValue a)



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
      else error "bad name for selector"
  gExactEither _ (Nothing, _) = error "bad name for selector"

instance (GRecordValue l, GRecordValue r) => GRecordValue (l :*: r) where
  gExactEither modifyField (_, v) =
    case v of
      M m ->
        let (fromList -> fstL, fromList -> sndL) = splitAt (length m `div` 2) $ toList m in
        (:*:) <$> gExactEither modifyField (Nothing, M fstL) <*> gExactEither modifyField (Nothing, M sndL)
      L l ->
        let (fstL, sndL) = splitAt (length l `div` 2) l in
        (:*:) <$> gExactEither modifyField (Nothing, L fstL) <*> gExactEither modifyField (Nothing, L sndL)
      _ -> error "bad structure"


instance (GRecordValue l, GRecordValue r) => GRecordValue (l :+: r) where
  gExactEither modifyField v =
    let res = L1 <$> gExactEither @l modifyField v in
    if isLeft res
      then res
      else R1 <$> gExactEither @r modifyField v

instance (RecordValue a) => GRecordValue (K1 i a) where
  gExactEither _ (_, v) = K1 <$> exactEither v

instance GRecordValue U1 where
  gExactEither _ (_, v) = if v == N () then Right U1 else error "fail with Constructor without arguments - it's not ()"
