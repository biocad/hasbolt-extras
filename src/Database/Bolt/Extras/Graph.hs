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

To make a Get query, you need to build a graph that you want to find in the DB.
For that, start with template nodes and relations like 'defaultNodeReturn' and 'defaultRelReturn'.
Customize them with endomorphisms in 'GetterLike' class and combine into template
graph 'Graph' using 'addNode' and 'addRelation'.

The result of running Get query will be represented as a 'Graph' as well. You can then use
convenient functions like 'extractNode' and 'extractRelation' to get your datatypes
(that are instances of 'Database.Bolt.Extras.NodeLike' or 'Database.Bolt.Extras.URelationshipLike')
from the result.

To make a Put query, describe your data as 'Graph', representing new nodes and relations as
'PutNode' and 'PutRelationship', and use 'makeRequest'. The query graph may also describe existing
nodes and relations, for example if you need to find a specific node in graph and attach a new one to
it, or update an existing node with new data.

Result of Put query will be graph with Neo4j ids of inserted data.

-}

module Database.Bolt.Extras.Graph
  (
    -- * Graph template construction
    Graph(..), vertices, relations,
    emptyGraph, addNode, addRelation,

    -- * Get queries
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
    extractNode, extractRelation,
    extractNodeId, extractRelationId,
    extractNodeAeson, extractRelationAeson,
    mergeGraphs,

    -- * Put queries
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
