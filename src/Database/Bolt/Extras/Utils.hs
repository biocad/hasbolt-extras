{-# OPTIONS_GHC -fno-warn-orphans #-}

module Database.Bolt.Extras.Utils
  (
    dummyId
  , union
  , currentLoc
  , exactValues
  , exactValuesM
  ) where

import           Control.Monad.IO.Class (MonadIO)
import           Data.List              (nub)
import           Data.Map.Strict        as M ((!), (!?))
import qualified Data.Map.Strict        as M (union)
import           Data.Text              (Text)
import           Database.Bolt          as B (BoltActionT, Node (..), Record,
                                              RecordValue, Value (..), exact)
import           Language.Haskell.TH    (Exp (..), Lit (..), Loc (..), Q,
                                         location)
import           Text.Printf            (printf)


-- | 'dummyId' is used to load 'Node' and 'URelationship' into database,
-- because id from database is not known for such moment.
--
dummyId :: Int
dummyId = -1

-- | 'Node's can be merged. 'union' is useful when you have to store in one node
-- several labels and props from different classes.
--
union :: Node -> Node -> Node
(Node _ labels1 props1) `union` (Node _ labels2 props2) = Node dummyId
                                                               (nub $ labels1 ++ labels2)
                                                               (props1 `M.union` props2)

-- | 'currentLoc' shows module name and line where this expression is used.
--
currentLoc :: Q Exp
currentLoc = do
  loc <- location
  pure $ LitE $ StringL $ printf "%s:%d: " (loc_module loc) (fst $ loc_start loc)

-- | Extract values
--
exactValues :: (Monad m, RecordValue a) => Text -> [Record] -> m [a]
exactValues var = mapM (exact . (! var))

-- | Extract values (maybe)
exactValuesM :: (MonadIO m, RecordValue a) => Text -> [Record] -> BoltActionT m [Maybe a]
exactValuesM var = mapM (safeExact . (!? var))
  where
    safeExact :: (MonadIO m, RecordValue a) => Maybe B.Value -> BoltActionT m (Maybe a)
    safeExact Nothing       = pure Nothing
    safeExact (Just (N ())) = pure Nothing
    safeExact (Just x )     = Just <$> exact x
