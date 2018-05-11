{-# LANGUAGE OverloadedStrings #-}

module Database.Bolt.Extras.DSL.Internal.Executer
  (
    formQuery
  ) where

import           Control.Monad.Free                          (Free (..),
                                                              foldFree)
import           Control.Monad.Writer                        (Writer,
                                                              execWriter, tell)
import           Data.Monoid                                 ((<>))
import           Data.Text                                   as T (Text,
                                                                   intercalate,
                                                                   unwords)
import           Database.Bolt.Extras.DSL.Internal.Instances ()
import           Database.Bolt.Extras.DSL.Internal.Types     (Expr (..))
import           Database.Bolt.Extras.Query.Cypher           (ToCypher (..))

-- | Translates 'Expr' to cypher query.
--
execute :: Expr a -> Writer [Text] a
execute (Create s n)        = executeHelperS "CREATE " s n
execute (Match s n)         = executeHelperS "MATCH " s n
execute (OptionalMatch s n) = executeHelperS "OPTIONAL MATCH " s n
execute (Merge s n)         = executeHelperS "MERGE " s n
execute (Where c n)         = executeHelperT "WHERE " c n
execute (Set t n)           = executeHelperT "SET " t n
execute (Delete t n)        = executeHelperT "DELETE " t n
execute (DetachDelete t n)  = executeHelperT "DETACH DELETE " t n
execute (Return t n)        = executeHelperT "RETURN " t n
execute (Text t n)          = tell [t] >> pure n

-- | Helper to translate 'Expr' with 'Selector's
--
executeHelperS :: ToCypher a => Text -> a -> b -> Writer [Text] b
executeHelperS txt s n = tell [txt <> toCypher s] >> pure n

-- | Helper to translate 'Expr' with 'Text's
--
executeHelperT :: Text -> [Text] -> b -> Writer [Text] b
executeHelperT txt t n = tell [txt <> intercalate ", " t] >> pure n

formQueryW :: Free Expr () -> Writer [Text] ()
formQueryW = foldFree execute

formQuery :: Free Expr () -> Text
formQuery = T.unwords . execWriter . formQueryW
