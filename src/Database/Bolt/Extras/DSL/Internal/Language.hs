module Database.Bolt.Extras.DSL.Internal.Language
  (
    CypherDSL
  , createF
  , matchF
  , optionalMatchF
  , mergeF
  , whereF
  , setF
  , deleteF
  , detachDeleteF
  , removeF
  , returnF
  , withF
  , textF
  ) where

import           Control.Monad.Free                      (Free (..), liftF)
import           Data.Text                               (Text)
import           Database.Bolt.Extras.DSL.Internal.Types (Conds (..), Expr (..),
                                                          Selectors)

-- | A synonym for 'Free' DSL.
--
type CypherDSL a = Free Expr a

-- | Prepare 'CREATE' query
--
createF :: Selectors -> Free Expr ()
createF sels = liftF (Create sels ())

-- | Prepare 'MATCH' query
--
matchF :: Selectors -> Free Expr ()
matchF sels = liftF (Match sels ())

-- | Prepare 'OPTIONAL MATCH' query
--
optionalMatchF :: Selectors -> Free Expr ()
optionalMatchF sels = liftF (OptionalMatch sels ())

-- | Prepare 'MERGE' query
--
mergeF :: Selectors -> Free Expr ()
mergeF sels = liftF (Merge sels ())

-- | Prepare 'WHERE' query
--
whereF :: Conds -> Free Expr ()
whereF conds = liftF (Where conds ())

-- | Prepare 'SET' query
--
setF :: [Text] -> Free Expr ()
setF txts = liftF (Set txts ())

-- | Prepare 'DELETE' query
--
deleteF :: [Text] -> Free Expr ()
deleteF txts = liftF (Delete txts ())

-- | Prepare 'DETACH DELETE' query
--
detachDeleteF :: [Text] -> Free Expr ()
detachDeleteF txts = liftF (DetachDelete txts ())

-- | Prepare 'REMOVE' query
--
removeF :: [Text] -> Free Expr ()
removeF txts = liftF (Remove txts ())

-- | Prepare 'RETURN' query
--
returnF :: [Text] -> Free Expr ()
returnF txts = liftF (Return txts ())

-- | Prepare 'WITH' query
--
withF :: [Text] -> Free Expr ()
withF txts = liftF (With txts ())

-- | Prepare query with custom text
--
textF :: Text -> Free Expr ()
textF txt = liftF (Text txt ())
