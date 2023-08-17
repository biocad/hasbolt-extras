{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Database.Bolt.Extras.DSL.Internal.Instances () where

import           Control.Monad.Writer                    (execWriter, tell)
import           Data.Function                           ((&))
import           Data.String                             (IsString(..))
import           Data.Proxy                              (Proxy (..))
import           Data.Text                               (intercalate, pack)
import           Database.Bolt.Extras                    (ToCypher (..),
                                                          fromInt)
import           GHC.OverloadedLabels                    (IsLabel (..))
import           GHC.TypeLits                            (KnownSymbol,
                                                          symbolVal)
import           NeatInterpolation                       (text)
import           Text.Printf                             (printf)

import           Database.Bolt.Extras.DSL.Internal.Types

instance KnownSymbol x => IsLabel x NodeSelector where
  fromLabel = defaultNode & withIdentifier (pack $ symbolVal @x Proxy)

instance KnownSymbol x => IsLabel x RelSelector where
  fromLabel = defaultRel & withIdentifier (pack $ symbolVal @x Proxy)

instance SelectorLike NodeSelector where
    withIdentifier idx node = node { nodeIdentifier = Just idx }
    withLabel lbl node      = node { nodeLabels = lbl : nodeLabels node }
    withProp prop node      = node { nodeProperties = prop : nodeProperties node }
    withParam prop node     = node { nodeParams = prop : nodeParams node }

instance SelectorLike RelSelector where
    withIdentifier idx rel = rel { relIdentifier = Just idx }
    withLabel lbl rel      = rel { relLabel = lbl }
    withProp prop rel      = rel { relProperties = prop : relProperties rel }
    withParam prop rel     = rel { relParams = prop : relParams rel }

instance ToCypher NodeSelector where
  toCypher NodeSelector{..} = execWriter $ do
    tell "("
    case nodeIdentifier of
      Just idx -> tell idx
      Nothing  -> pure ()
    case nodeLabels of
      [] -> pure ()
      _  -> tell $ toCypher nodeLabels
    case nodeProperties of
      [] -> case nodeParams of
              [] -> pure ()
              _ -> do
                tell "{"
                tell $ toCypher nodeParams
                tell "}"
      _ -> do
        tell "{"
        tell $ toCypher nodeProperties
        case nodeParams of
          [] -> pure ()
          _ -> do
            tell ","
            tell $ toCypher nodeParams
        tell "}"

    tell ")"

instance ToCypher RelSelector where
  toCypher RelSelector{..} = execWriter $ do
    tell "["
    case relIdentifier of
      Just idx -> tell idx
      Nothing  -> pure ()
    case relLabel of
      "" -> pure ()
      _  -> tell $ toCypher relLabel
    case relProperties of
      [] -> case relParams of
              [] -> pure ()
              _ -> do
                tell "{"
                tell $ toCypher relParams
                tell "}"
      _ -> do
        tell "{"
        tell $ toCypher relProperties
        case relParams of
          [] -> pure ()
          _ -> do
            tell ","
            tell $ toCypher relParams
        tell "}"

    tell "]"

instance ToCypher PathSelector where
  toCypher (ps :-!: rs :!->: ns) = execWriter $ do
    tell $ toCypher ps
    tell "-"
    tell $ toCypher rs
    tell "->"
    tell $ toCypher ns
  toCypher (ps :<-!: rs :!-: ns) = execWriter $ do
    tell $ toCypher ps
    tell "<-"
    tell $ toCypher rs
    tell "-"
    tell $ toCypher ns
  toCypher (ps :-!: rs :!-: ns) = execWriter $ do
    tell $ toCypher ps
    tell "-"
    tell $ toCypher rs
    tell "-"
    tell $ toCypher ns
  toCypher (P ns) = execWriter $
    tell $ toCypher ns
  toCypher (_ :<-!: _ :!->: _) = error "Database.Bolt.Extras.DSL: incorrect path"

instance ToCypher Selector where
  toCypher (PS ps)  = toCypher ps
  toCypher (TS txt) = txt

instance ToCypher Selectors where
  toCypher = intercalate ", " . fmap toCypher

instance IsString Cond where
  fromString str = TC $ pack str

instance ToCypher Cond where
  toCypher (ID t bId)   = pack $ printf "ID(%s)=%d" t (fromInt bId)
  toCypher (IDs t bIds) = pack $ printf "ID(%s) in [%s]" t (intercalate ", " $ fmap (pack . show) bIds)
  toCypher (IN t txts)  = pack $ printf "%s in [%s]" t (intercalate ", " $ fmap (\s -> [text|"$s"|]) txts)
  toCypher (TC txt)     = txt

instance IsString Conds where
  fromString str = C $ TC $ pack str

instance ToCypher Conds where
  -- Adding "(" ")" to have correct precedence in Cypher
  -- "(a :&&: b) :||: c" should mean "(a AND b) OR c", not "a AND b OR c"
  toCypher (fcp :&&: scp) = "(" <> toCypher fcp <> ") AND (" <> toCypher scp <> ")"
  toCypher (fcp :||: scp) = "(" <> toCypher fcp <> ") OR (" <> toCypher scp <> ")"
  toCypher (Not cp)       = "NOT (" <> toCypher cp <> ")"
  toCypher (C cp)         = toCypher cp
