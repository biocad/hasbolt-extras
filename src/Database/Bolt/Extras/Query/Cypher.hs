{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Database.Bolt.Extras.Query.Cypher
  (
    ToCypher (..)
  ) where

-------------------------------------------------------------------------------------------------
-- Queries for neo4j are formatted with `Cypher` language.
-- Read documentation for `Cypher` here: https://neo4j.com/docs/developer-manual/current/cypher/.
-- This file contains some converation rules from 'Database.Bolt' types to `Cypher`.
-------------------------------------------------------------------------------------------------

import           Data.Text                         as T (Text, concat, cons,
                                                         intercalate, pack,
                                                         toUpper)
import           Database.Bolt                     (Value (..))
import           Database.Bolt.Extras.Template     (Label, Property)
import           Database.Bolt.Extras.Utils        (currentLoc)
import           NeatInterpolation                 (text)

-- | The class for convertation into Cypher.
--
class ToCypher a where
  toCypher :: a -> Text

-- | Convertation for 'Database.Bolt.Value' into Cypher.
--
instance ToCypher Value where
  toCypher (N ())     = ""
  toCypher (B bool)   = toUpper . pack . show $ bool
  toCypher (I int)    = pack . show $ int
  toCypher (F double) = pack . show $ double
  toCypher (T t)      = [text|"$t"|]
  toCypher (L values) = let cvalues = T.intercalate "," $ map toCypher values
                        in [text|[$cvalues]|]
  toCypher _          = error $ $currentLoc ++ "unacceptable Value type"

-- | Label with @name@ are formatted into @:name@
--
instance ToCypher Label where
  toCypher = cons ':'

-- | Several labels are formatted with concatenation.
--
instance ToCypher [Label] where
  toCypher = T.concat . map toCypher

-- | Converts property with @name@ and @value@ to @name:value@.
--
instance ToCypher Property where
  toCypher (propTitle, value) = T.concat [propTitle, pack ":", toCypher value]

-- | Several properties are formatted with concatenation.
--
instance ToCypher [Property] where
  toCypher = T.intercalate "," . map toCypher

