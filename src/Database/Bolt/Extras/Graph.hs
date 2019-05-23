{-|

This module defines everything needed to make template graph requests to Neo4j.

There are two types of queries that you can run: queries that return something from the
database (Get) and queries that save new data to it (Put). Both types are abstracted via type class
'GraphQuery'. Most of the time you will need only its 'makeRequest' method.

Get and Put queries are simply two instances of 'GraphQuery', differentiated by empty data
types 'GetRequest' and 'PutRequest'. This means that you will have to use @TypeApplications@
to call 'GraphQuery' methods, like this:

> makeRequest @GetRequest ...

All queries are built from simple templates that can be customized with endomorphisms
(things of type @a -> a@, like the Builder pattern in OOP).
Endomorphisms can be conveniently applied using 'Data.Function.&' operator.

A complete example of running Get and Put queries can be found in "@example/Main.hs@" file in this
repository.

-}

module Database.Bolt.Extras.Graph
  (
    -- * Graph template construction
    -- | Both query types require a 'Graph' type. Preffered way to create a variable of this type
    -- is to start with 'emptyGraph' and add required nodes and relations with 'addNode' and
    -- 'addRelation' function.
    --
    -- For example (using @Text@ as node data for simplicity):
    --
    -- > queryG :: Graph Text Text Text
    -- > queryG = emptyGraph
    -- >   & addNode "a" "node a"
    -- >   & addNode "b" "node b
    -- >   & addRelation "a" "b" "relation a -> b"
    --
    Graph(..), vertices, relations,
    emptyGraph, addNode, addRelation,

    -- * Get queries
    -- | Get queries are represented by 'GraphGetRequest' type - it is a 'Graph' filled with templates
    -- for nodes and relations: 'NodeGetter' and 'RelGetter'.
    --
    -- To make a query, you need to build a template of graph that you want to find in the DB.
    -- For that, start with empty nodes and relations like 'defaultNodeReturn' and 'defaultRelReturn'.
    -- Customize them with endomorphisms in 'GetterLike' class and combine into template
    -- graph 'Graph' using 'emptyGraph', 'addNode' and 'addRelation'.
    --
    -- Typically, a node template is constructed like this:
    --
    -- > defaultNodeReturn
    -- >   & withLabelQ ''NodeType
    -- >   & withBoltId nodeId
    -- >   & withReturn allProps
    --
    -- The result of running Get query will be represented as a 'Graph' as well, with 'GraphGetResponse'
    -- alias. You can then use convenient functions like 'extractNode' and 'extractRelation' to get
    -- your datatypes (that are instances of 'Database.Bolt.Extras.NodeLike'
    -- or 'Database.Bolt.Extras.URelationshipLike') from the result.

    -- ** Getter types
    GetRequest,
    GetterLike(..),
    NodeGetter(..), RelGetter(..),
    GraphGetRequest,

    -- ** Default getters
    defaultNode, defaultNodeReturn, defaultNodeNotReturn,
    defaultRel, defaultRelReturn, defaultRelNotReturn,
    allProps,

    -- ** Result types
    NodeResult(..), RelResult(..),
    GraphGetResponse,

    -- ** Extracting result
    -- | These functions are for extracting nodes and relations in various formats.
    -- If an entity does not exist in given 'GraphGetResponse' or is of invalid type,
    -- an @error@ will be thrown.
    --
    -- For example, assume you have this query:
    --
    -- @
    --   queryG :: GraphGetRequest
    --   queryG = emptyGraph
    --     & addNode "exNode"
    --       (defaultNodeReturn
    --          & withLabelQ ''ExampleNode
    --          & withProp   ("exampleFieldT", T "A")
    --          & withReturn allProps
    --       )
    -- @
    --
    -- And run it:
    --
    -- > result <- makeRequest @GetRequest [] queryG
    --
    -- Then you can get @ExampleNode@ value from the result
    --
    -- > let nodes = map extractNode "exNode" result :: [ExampleNode]
    --
    -- You can also just ask for an id of node:
    --
    -- > let nodeIds = map extractNodeId "exNode" result
    --
    -- Or, if you did not use @withReturn allProps@, you can use 'extractNodeAeson' to get raw
    -- 'NodeResult' value and inspect its properties.
    extractNode, extractRelation,
    extractNodeId, extractRelationId,
    extractNodeAeson, extractRelationAeson,
    mergeGraphs,

    -- * Put queries
    -- | Put queries are represented with 'GraphPutRequest' - a 'Graph' of 'PutNode' and 'PutRelationship'.
    -- Build your graph the same way as with Get queryб representing new nodes and relations as
    -- 'PutNode' and 'PutRelationship'. The query graph may also describe existing
    -- nodes and relations, for example if you need to find a specific node in graph and attach a new one to
    -- it, or update an existing node with new data.
    --
    -- Result of Put query will be graph with Neo4j ids of inserted data.
    PutRequest,
    PutNode(..), PutRelationship(..),
    GraphPutRequest, GraphPutResponse,

    -- * Internal machinery for forming Cypher queries
    GraphQuery(..),
    Requestable(..), Returnable(..), Extractable(..),
    NodeName, relationName,
    requestGetters, requestPut,

    (#),
  ) where

import           Database.Bolt.Extras.Graph.Internal.AbstractGraph
import           Database.Bolt.Extras.Graph.Internal.Class
import           Database.Bolt.Extras.Graph.Internal.Get
import           Database.Bolt.Extras.Graph.Internal.GraphQuery
import           Database.Bolt.Extras.Graph.Internal.Put
