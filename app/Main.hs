{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE DeriveFunctor       #-}

module Main where

import           Control.Exception (bracket)
import           Data.Default      (def)
import           Database.Bolt
import Data.Text as T
import           Control.Monad.IO.Class         (MonadIO)
import           Data.Map.Strict                     (Map, keys, mapWithKey,
                                                      toList, (!))
import NeatInterpolation
import Database.Bolt.Extras (ToValue (..), ToCypher (..), Property (..))
import Data.Monoid ((<>))
import Control.Monad.Writer
import Control.Monad.Free (Free (..), foldFree, liftF, iter)

myConfiguration :: BoltCfg
myConfiguration = def { user = "neo4j", password = "12345" }

main :: IO ()
main = bracket
         (connect myConfiguration)
         close
         (\pipe -> do res <- run pipe foo
                      print res)

foo :: MonadIO m => BoltActionT m [Node]
foo = do
      records <- query $ intercalate " " $ execWriter q
      exactValues "n" records

exactValues :: MonadIO m => Text -> [Record] -> BoltActionT m [Node]
exactValues var = mapM (exact . (! var))

matchQ :: Text -> Text -> Text
matchQ t = append ("MATCH " `append` "(" `append` toCypher t `append` ")")

returnQ :: Text -> Text -> Text
returnQ t = append (" RETURN " `append` t)

whereQ :: Text -> Text -> Text
whereQ t = append [text| WHERE ($t)|]

--q :: Text
--q = mkQ $ matchQ ("n", [], [("systemName", toValue (T.pack "IL23A"))]) . returnQ "n"

endQ :: Text
endQ = ""

mkQ :: (Text -> Text) -> Text
mkQ q = q endQ

--type Property = (Text, Value)

--instance ToCypher Property where
--  toCypher (propTitle, value) = T.concat [propTitle, T.pack ":", toCypher value]

--instance ToValue a => ToCypher [(Property a)] where
--  toCypher = T.intercalate "," . Prelude.map toCypher

--type NodeSelector = (Text, [Text], [Property])

--type RelSelector = (NodeSelector, (Text, Text, [Property]), NodeSelector)

data Selector = NS NodeSelector | RS RelSelector | TS Text
  deriving (Show, Eq)

instance ToCypher Selector where
  toCypher (NS ns) = toCypher ns
  toCypher (RS rs) = toCypher rs
  toCypher (TS txt) = txt

--data Start = Create [Selector] | Match [Selector] | OptMatch [Selector] | Merge [Selector]

type Cond = [Text]

data Expr next = Create [Selector] next
               | Match [Selector] next
               | OptMatch [Selector] next
               | Merge [Selector] next 
               | Where Cond next
               | Return [Text] Text next
               | Text next
  deriving (Show, Eq, Functor)

create :: [Selector] -> Free Expr ()
create sels = liftF (Create sels ())

match :: [Selector] -> Free Expr ()
match sels = liftF (Match sels ())

return' :: [Text] -> Text -> Free Expr ()
return' txts txt = liftF (Return txts txt ())

f :: Free Expr ()
f = match [NS sampleNode] >> return' ["n"] "" >> return' ["n"] ""

execute :: Expr a -> Writer [Text] a
execute m@(Match _ n) = do tell [toCypher m] >> pure n
execute (Create s n) = tell ["CREATE"] >> pure n
execute r@(Return _ _ n) = tell [toCypher r] >> pure n


q :: Writer [Text] ()
q = foldFree execute f




--expr :: Expr (BoltActionT m ()) -> BoltActionT m ()
{-
data Nat nat = Zero | Succ nat

--Create

{-
Zero :: Nat a
Succ Zero :: Nat (Nat a)
-}

newtype Fix f = Fix { unFix :: f (Fix f) }

data N = Fix Nat

nalg :: Nat Int -> Int
nalg Zero     = 0
nalg (Succ x) = x + 1

-- Succ (Succ (Succ (Succ Zero))) :: Nat (Nat (Nat (Nat a)))
-- Fix $ Succ (Fix $ Succ (Fix $ Succ (Fix $ Succ (Fix Zero)))) :: N

nalg (Succ 3) == 4
nalg (Succ 2) == 3
nalg (Succ 1) == 2
nalg (Zero)   == 1

instance Functor Nat where
  fmap _ Z     = Z
  fmap f (S x) = S (f x)

cata :: Functor f => (f a -> a) -> Fix f -> a
cata fa (Fix x) = fa $ fmap (cata fa) x
--           |
--           +- f (Fix f)
--  fmap (..) :: f (Fix f) -> f a
--        |
--        +- (Fix f) -> a
-}


instance ToCypher (Expr next) where
  toCypher (Match sels _) = "MATCH " <> (intercalate ", " $ fmap toCypher sels) <> " "
  toCypher (Return txts txt _) = "RETURN " <> (intercalate ", " txts) <> " " <> txt

data NodeSelector = NodeSelector { nodeIdentifier :: Maybe Text
                                 , nodeLabels     :: [Text]
                                 , nodeProps      :: [(Text, Value)]
                                 }
  deriving (Show, Eq)


data RelSelector = RelSelector { relStartNode  :: NodeSelector
                               , relEndNode    :: NodeSelector
                               , relIdentifier :: Maybe Text
                               , relLabel      :: Text
                               , relProps      :: [(Text, Value)]
                               }
  deriving (Show, Eq)

(#) :: a -> (a -> b) -> b
(#) = flip ($)

withIdentifier :: Text -> NodeSelector -> NodeSelector
withIdentifier idx node = node { nodeIdentifier = Just idx }

addLabel :: Text -> NodeSelector -> NodeSelector
addLabel lbl node = node { nodeLabels = lbl : nodeLabels node }

defaultNode :: NodeSelector
defaultNode = NodeSelector Nothing [] []

sampleNode :: NodeSelector
sampleNode = defaultNode # withIdentifier "n"

instance ToCypher NodeSelector where
  toCypher NodeSelector{..} = execWriter $ do
      tell "("
      case nodeIdentifier of
        Just idx -> tell idx
        Nothing  -> pure ()
      case nodeLabels of
        [] -> pure ()
        _  -> do tell $ toCypher nodeLabels
      case nodeProps of
        [] -> pure ()
        _  -> do tell "{"
                 tell $ toCypher nodeProps
                 tell "}"
      tell ")"

instance ToCypher RelSelector where
  toCypher RelSelector{..} = execWriter $ do
      tell $ toCypher relStartNode
      tell "-[" 
      case relIdentifier of
        Just idx -> tell idx
        Nothing  -> pure ()
      case relLabel of
        "" -> pure ()
        _     -> do tell $ toCypher relLabel
      case relProps of
        [] -> pure ()
        _  -> do tell "{"
                 tell $ toCypher relProps
                 tell "}"
      tell "]-"
      tell $ toCypher relEndNode

-- 
{-
instance ToCypher NodeSelector where
  toCypher (t, [], []) = "(" <> t <> ")"
  toCypher (t, x, [])  = "(" <> t <> " " <> toCypher x <> ")"
  toCypher (t, [], m)  = "(" <> t <> " {" <> toCypher m <> "}" <> ")"
  toCypher (t, x, m)   = "(" <> t <> " " <> toCypher x <> " {" <> toCypher m <> "}" <> ")"

instance ToCypher RelSelector where
  toCypher (st, (n, empty, []), end) = toCypher st <> "-[" <> n <> "]-" <> toCypher end
  toCypher (st, (n, l, []), end) = toCypher st <> "-[" <> n <> toCypher l <> "]-" <> toCypher end
  toCypher (st, (n, empty, m), end) = toCypher st <> "-[" <> n <>  " {" <> toCypher m <> "} " <> "]-" <> toCypher end
  toCypher (st, (n, l, m), end) = toCypher st <> "-[" <> n <> toCypher l <> " {" <> toCypher m <> "} " <> "]-" <> toCypher end
-}
--MATCH (sys:System)-[:INCLUDE_MOLECULE]-(:Molecule)-[:INCLUDE_SEQUENCE]-(:Sequence)-[:MARKING_RANGE_OF]-(mark:Marking)--
--RETURN sys, mark


--MATCH (sys:System)-[sys0mol:INCLUDE_MOLECULE]-(mol:Molecule),
--      (mol:Molecule)-[mol0seq:INCLUDE_SEQUENCE]-(seq:Sequence),
--      (seq:Sequence)-[seq0mark:MARKING_RANGE_OF]-(mark:Marking)
--RETURN sys,mark

