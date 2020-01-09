{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

module Database.Bolt.Extras.DSL.Typed.Parameters
  where

import           Control.Monad.IO.Class                     (MonadIO)
import           Data.Kind                                  (Type)
import qualified Data.Map.Strict                            as Map
import           Data.Text                                  (Text, pack)
import           Database.Bolt                              (BoltActionT,
                                                             IsValue (..),
                                                             Record, Value,
                                                             queryP)
import           GHC.TypeLits                               (Symbol)

import           Database.Bolt.Extras.DSL.Internal.Executer (formQuery)
import           Database.Bolt.Extras.DSL.Internal.Language (CypherDSL)
import           Database.Bolt.Extras.DSL.Typed.Types       (SymbolS (..))

{- $setup
>>> :set -XTypeApplications
>>> :set -XOverloadedLabels
>>> :set -XDataKinds
>>> :set -XOverloadedStrings
>>> :load Database.Bolt.Extras.DSL.Typed.Instances Database.Bolt.Extras.DSL.Typed.Parameters
>>> import Control.Monad.IO.Class
>>> import Database.Bolt (BoltActionT, Record)
>>> import Database.Bolt.Extras.DSL (returnF)
>>> import Database.Bolt.Extras.DSL.Typed.Parameters
-}

-- | A wrapper around arbitrary 'CypherDSL' expression which stores type-level list of named
-- parameters (@$foo@) with their types.
newtype CypherDSLParams (params :: [(Symbol, Type)]) (a :: Type)
  = CypherDSLParams (CypherDSL a)

-- | This type class ensures safety of queries with parameters by checking in compile time that
-- all parameters are supplied and have correct type.
--
-- Instances of this class will add more arguments to @fun@, one for each element in @params@.
--
-- This should be considered an implementation detail.
class QueryWithParams (params :: [(Symbol, Type)]) (m :: Type -> Type) fun | params m -> fun where
  -- | Internal function that accumulates parameters from type-level list.
  collectParams :: CypherDSL () -> [(Text, Value)] -> fun

-- | Base case: if there are no parameters, perform query with 'queryP'.
instance MonadIO m => QueryWithParams '[] m (BoltActionT m [Record]) where
  collectParams dsl params = queryP (formQuery dsl) $ Map.fromList params

-- | Recursion case: append the next parameter to accumulator and to function's type.
instance (IsValue typ, QueryWithParams rest m fun)
      => QueryWithParams ('(field, typ) ': rest) m ((SymbolS field, typ) -> fun)
  where

  collectParams dsl params (SymbolS s, a) = collectParams @rest @m dsl ((pack s, toValue a) : params)

-- | Run a query (in the form of 'CypherDSLParams'). This is a function of variable number of arguments.
-- Actual number will be determined by type-level list @params@.
--
-- A couple of examples:
--
-- >>> dsl = CypherDSLParams (returnF []) :: CypherDSLParams '[ '("foo", Int), '("bar", Text) ] ()
-- >>> :t queryWithParams dsl
-- queryWithParams dsl
--   :: MonadIO m =>
--      (SymbolS "foo", Int)
--      -> (SymbolS "bar", Text) -> BoltActionT m [Record]
-- >>> :t queryWithParams dsl (#foo =: 42)
-- queryWithParams dsl (#foo =: 42)
--   :: MonadIO m => (SymbolS "bar", Text) -> BoltActionT m [Record]
-- >>> :t queryWithParams dsl (#foo =: 42) (#bar =: "Hello")
-- queryWithParams dsl (#foo =: 42) (#bar =: "Hello")
--   :: MonadIO m => BoltActionT m [Record]
-- >>> :t queryWithParams dsl (#foo =: True)
-- ...
-- ... Couldn't match type ‘Int’ with ‘Bool’
-- ...
-- >>> :t queryWithParams dsl (#bar =: 42)
-- ...
-- ... Couldn't match type ‘"bar"’ with ‘"foo"’
-- ...
queryWithParams
  :: forall params m fun
  .  MonadIO m
  => QueryWithParams params m fun
  => CypherDSLParams params ()
  -> fun
queryWithParams (CypherDSLParams dsl) = collectParams @params @m dsl []
