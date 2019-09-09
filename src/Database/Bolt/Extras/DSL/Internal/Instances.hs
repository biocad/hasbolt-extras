{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Database.Bolt.Extras.DSL.Internal.Instances where

import           Data.Text                               (intercalate, pack)
import           Database.Bolt.Extras                    (ToCypher (..),
                                                          fromInt)
import           Database.Bolt.Extras.Selector           (Requestable (..),
                                                          Selector (..),
                                                          Selectors)
import           Text.Printf                             (printf)

import           Database.Bolt.Extras.DSL.Internal.Types (Cond (..), Conds (..))

instance ToCypher Selectors where
  toCypher a = intercalate ", " $ map (\(Selector s) -> request s) a

instance ToCypher Cond where
  toCypher (ID t bId)   = pack $ printf "ID(%s)=%d" t (fromInt bId)
  toCypher (IDs t bIds) = pack $ printf "ID(%s) in [%s]" t (intercalate ", " $ fmap (pack . show) bIds)
  toCypher (IN t txts)  = pack $ printf "%s in [%s]" t (intercalate ", " $ fmap (\s -> "\"" <> s <> "\"") txts)
  toCypher (TC txt)     = txt

instance ToCypher Conds where
  toCypher (fcp :&&: scp) = toCypher fcp <> " AND " <> toCypher scp
  toCypher (fcp :||: scp) = toCypher fcp <> " OR " <> toCypher scp
  toCypher (Not cp)       = "NOT " <> toCypher cp
  toCypher (C cp)         = toCypher cp
