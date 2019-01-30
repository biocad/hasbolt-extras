{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}

module Main where

import           Control.Exception             (bracket)
import           Control.Monad.State           (execState, modify)
import           Data.Aeson                    (encode)
import qualified Data.ByteString.Lazy.Char8    as B (putStrLn)
import           Data.Default                  (def)
import           Data.Text                     (Text)
import           Database.Bolt                 (BoltActionT, BoltCfg (..),
                                                Value (..), close, connect, run)
import           Database.Bolt.Extras          (NodeLike (..),
                                                URelationLike (..))
import           Database.Bolt.Extras.Graph
import           Database.Bolt.Extras.Template (makeNodeLike, makeURelationLike)
import           GHC.Generics                  (Generic)

-- | Configuration for connection to local database.
boltCfg :: BoltCfg
boltCfg = def { host = "localhost"
              , user = "neo4j"
              , password = "12345"
              }

-- | Helper to run queries in Neo4j DB.
--
runQueryDB :: BoltActionT IO a -> IO a
runQueryDB act = bracket (connect boltCfg) close (`run` act)

data ExampleNode = ExampleNode { exampleFieldT :: Text
                               , exampleFieldI :: Int
                               }
  deriving (Show, Generic)

data EXAMPLE_RELATION = EXAMPLE_RELATION
  deriving (Show, Generic)

makeNodeLike ''ExampleNode
makeURelationLike ''EXAMPLE_RELATION

exNodeAVar :: Text
exNodeAVar = "nodeA"

exNodeBVar :: Text
exNodeBVar = "nodeB"

-- | Builds query:
-- CREATE (nodeA:ExampleNode { exampleFieldT: "A" , exampleFieldI: 1}) WITH nodeA
-- MERGE  (nodeB:ExampleNode { exampleFieldT: "B" , exampleFieldI: 2}) WITH nodeB
-- MERGE  (nodeA)-[nodeA0nodeB:EXAMPLE_RELATION]-(nodeB) WITH nodeA, nodeB, nodeA0nodeB
-- RETURN ID(nodeA), ID(nodeB), ID(nodeA0nodeB)
--
examplePutGraph :: GraphPutRequest
examplePutGraph = flip execState emptyGraph $ do
    modify $ addNode exNodeAVar (CreateN . toNode $ exNodeA)
    modify $ addNode exNodeBVar (MergeN  . toNode $ exNodeB)

    modify $ addRelation exNodeAVar exNodeBVar (MergeR . toURelation $ exRel)
  where
    exNodeA = ExampleNode "A" 1
    exNodeB = ExampleNode "B" 2
    exRel   = EXAMPLE_RELATION

-- | Builds query:
-- MATCH (nodeA)-[:EXAMPLE_RELATION]-(nodeB),
-- (nodeA:ExampleNode { exampleField: "A" }),
-- (nodeB:ExampleNode { exampleField: "B" })
-- RETURN { id: ID(nodeA), labels: labels(nodeA), props: properties(nodeA) }
--
exampleGetGraphB :: GraphGetRequest
exampleGetGraphB = flip execState emptyGraph $ do
    modify $ addNode exNodeAVar exNodeA
    modify $ addNode exNodeBVar exNodeB

    modify $ addRelation exNodeAVar exNodeBVar exRel
  where
    exNodeA = defaultNodeReturn    # withLabelQ ''ExampleNode
                                   # withProp   ("exampleFieldT", T "A")
                                   # withReturn allProps

    exNodeB = defaultNodeNotReturn # withLabelQ ''ExampleNode
                                   # withProp   ("exampleFieldT", T "B")

    exRel   = defaultRelNotReturn  # withLabelQ ''EXAMPLE_RELATION

-- | Builds query:
-- MATCH (nodeA:ExampleNode { exampleField: "A" })
-- RETURN { id: ID(nodeA), labels: labels(nodeA), props: nodeA {.exampleFieldI} }
--
exampleGetGraphA :: GraphGetRequest
exampleGetGraphA = flip execState emptyGraph $
    modify $ addNode exNodeAVar exNodeA
  where
    exNodeA = defaultNodeReturn    # withLabelQ ''ExampleNode
                                   # withProp   ("exampleFieldT", T "A")
                                   # withReturn ["exampleFieldI"]

-- | Put 'examplePutGraph' to the database.
--
putGraph :: IO ()
putGraph = do
    putGraphR <- runQueryDB $ makeRequest @PutRequest [] examplePutGraph
    putStrLn "Uploaded graph: "
    print putGraphR

-- Get 'exampleGetGraphB' and parse it to Haskell object.
--
getGraphB :: IO ()
getGraphB = do
    getGraphsR                  <- runQueryDB $ makeRequest @GetRequest [] exampleGetGraphB
    let nodesA :: [ExampleNode] = extractNode exNodeAVar <$> getGraphsR
    putStrLn "Downloaded graph and converted to Haskell object: "
    print nodesA

-- Get 'exampleGetGraphA' and parse it to JSON.
--
getGraphA :: IO ()
getGraphA = do
    getGraphsR <- runQueryDB $ makeRequest @GetRequest [] exampleGetGraphA
    let nodesA = extractNodeAeson exNodeAVar <$> getGraphsR
    putStrLn "Downloaded graph and converted to JSON: "
    B.putStrLn . encode $ nodesA

main :: IO ()
main = putGraph >> getGraphB >> getGraphA
