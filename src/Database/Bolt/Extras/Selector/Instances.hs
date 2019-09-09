{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Database.Bolt.Extras.Selector.Instances where

import           Data.Map.Strict                        (insert, toList)
import           Data.Text                              (Text, pack)
import           Database.Bolt.Extras                   (ToCypher (..))
import           Language.Haskell.TH.Syntax             (nameBase)
import           NeatInterpolation                      (text)

import           Data.Proxy
import           GHC.OverloadedLabels
import           GHC.TypeLits

import           Database.Bolt.Extras.Selector.Class    (GetterLike (..),
                                                         Requestable (..))
import           Database.Bolt.Extras.Selector.Defaults (defaultNodeReturn,
                                                         defaultRelReturn)
import           Database.Bolt.Extras.Selector.Path     (PathEnd (..),
                                                         PathSelector (..))
import           Database.Bolt.Extras.Selector.Types    (NodeGetter (..),
                                                         NodeName,
                                                         RelGetter (..),
                                                         RelName)

instance KnownSymbol x => IsLabel x (NodeName, NodeGetter) where
  fromLabel = (pack $ symbolVal @x Proxy, defaultNodeReturn)

instance KnownSymbol x => IsLabel x (NodeName, RelGetter) where
  fromLabel = (pack $ symbolVal @x Proxy, defaultRelReturn)

instance GetterLike NodeGetter where
    withBoltId boltId ng = ng { ngboltId       = Just boltId }
    withLabel  lbl    ng = ng { ngLabels       = lbl : ngLabels ng }
    withLabelQ lblQ      = withLabel (pack . nameBase $ lblQ)
    withProp (pk, pv) ng = ng { ngProps        = insert pk pv (ngProps ng) }
    withReturn props  ng = ng { ngReturnProps  = ngReturnProps ng ++ props }
    isReturned        ng = ng { ngIsReturned   = True }

instance GetterLike RelGetter where
    withBoltId boltId rg = rg { rgboltId       = Just boltId }
    withLabel  lbl    rg = rg { rgLabel        = Just lbl    }
    withLabelQ lblQ      = withLabel (pack . nameBase $ lblQ)
    withProp (pk, pv) rg = rg { rgProps        = insert pk pv (rgProps rg) }
    withReturn props  rg = rg { rgReturnProps  = rgReturnProps rg ++ props }
    isReturned        rg = rg { rgIsReturned   = True }

instance Requestable (NodeName, NodeGetter) where
  request (name, ng) = [text|($name $labels $propsQ)|]
    where
      labels = toCypher . ngLabels $ ng
      propsQ = "{" <> (toCypher . toList . ngProps $ ng) <> "}"

instance Requestable (RelName, RelGetter) where
  request (name, rg) = [text|[$name $typeQ $propsQ]|]
    where
      typeQ  = maybe "" toCypher (rgLabel rg)
      propsQ = "{" <> (toCypher . toList . rgProps $ rg) <> "}"

instance Requestable ((NodeName, NodeName), RelGetter) where
  request ((stName, enName), rg) = [text|($stName)-[$name $typeQ $propsQ]->($enName)|]
    where
      name   = relationName (stName, enName)
      typeQ  = maybe "" toCypher (rgLabel rg)
      propsQ = "{" <> (toCypher . toList . rgProps $ rg) <> "}"

instance Requestable (PathSelector 'NodeEnd) where
  request (PStartNode ns) = request ns
  request (PEndNode ps ns) =
    case ps of
      PDirected ps' rs -> request ps' <> "-" <> request rs <> "->" <> request ns
      PUndirected ps' rs -> request ps' <> "-" <> request rs <> "-" <> request ns

-- | Build relationship name from the names of its start and end nodes
-- like @[startNodeName]0[endNodeName]@.
relationName :: (NodeName, NodeName) -> Text
relationName (st, en) = st <> "0" <> en
