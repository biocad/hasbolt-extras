{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Database.Bolt.Extras.Query.Temp where

-- @def@ from package data-default
import           Data.Default                        (def)
import           Data.Map.Strict                     (fromList)
import qualified Data.Map.Strict                     as M (Map, adjust, assocs,
                                                           keys, map, mapKeys,
                                                           mapWithKey, toList,
                                                           (!))
import qualified Data.Text                           as T (Text, pack, empty)
import           Database.Bolt
import           Database.Bolt.Extras.Query
import           Database.Bolt.Extras.Template       (makeNodeLike,
                                                      makeURelationLike)
import           Database.Bolt.Extras.Template.Types
import           Database.Bolt.Extras.Utils
import           Database.Bolt.Id                    (BoltId (..),
                                                      GetBoltId (..), fromInt)
import           Text.Printf                         (printf)
import           Database.Bolt.Extras.Template.Types
import           Database.Bolt.Extras.Template.Instances


-- | this function just run prepared query and return answer
runQ :: BoltActionT IO a -> IO a
runQ action = do
    pipe   <- connect boltCfg
    result <- run pipe action
    close pipe
    pure result
  where
    boltCfg = def { user     = "neo4j"     -- database login, neo4h by default
                  , password = "123"   -- password for database
                  , host     = "localhost" -- host with database
                  , port     = 7687        -- standart port
                  }

newtype Smth = Smth { someInt :: Int }
  deriving (Show)

newtype DoubleSmth = DoubleSmth { smthInt :: Int }
  deriving (Show)

newtype THIS_IS_RELATION = THIS_IS_RELATION { relType :: Int }
  deriving (Show)

data THIS_IS_ANOTHER_RELATION = THIS_IS_ANOTHER_RELATION
  deriving (Show)

makeNodeLike ''Smth
makeNodeLike ''DoubleSmth
makeURelationLike ''THIS_IS_RELATION
makeURelationLike ''THIS_IS_ANOTHER_RELATION

makeU :: Relationship -> URelationship
makeU Relationship{..} = URelationship relIdentity relType relProps

runWithoutClass :: IO ()
runWithoutClass = do
  {-let sel1 = NodeSelector Nothing (Just ["VariableDomainScoring"]) (Just [("chainType", (toValue (T.pack "K")))])
  let sel2 = URelSelector (Just "SCORING_RANGE_OF") Nothing
  let sel3 = NodeSelector Nothing (Just ["Sequence"]) Nothing
  let sel4 = NodeSelector Nothing (Just ["Structure"]) Nothing
  let sel5 = URelSelector (Just "INCLUDE_STRUCTURE") Nothing
  let graphSel = Graph (fromList [("f", sel1), ("s", sel3), ("t", sel4)])
                       (fromList [(("f", "s"), sel2), (("s", "t"), sel5)])
  result <- runQ $ (getGraph ["s0t.end = f0s.sEnd", "s0t.start = f0s.sStart"] graphSel)
  print (map (length . _vertices) result)
  --print result
  -}
  let t = Create (toNode $ DoubleSmth 1)
  let e = Create (toNode $ DoubleSmth 2)

  let r1 = toURelation $ THIS_IS_RELATION 20
  let f = NodeSelector Nothing (Just ["Smth"]) (Just [("someInt", (I 1))])
  let s = NodeSelector Nothing (Just ["Smth"]) (Just [("someInt", (I 2))])
  let r = URelSelector (Just "THIS_IS_RELATION") Nothing
  --modify $ addNode "f" f
  --modify $ addNode "s" s
  --modify $ addRelation "f" "s" r
  let nodes = fromList [("f", t), ("s", e)]
  let rels = fromList [(("f", "s"), r1)]
  let graph = Graph nodes rels
  result <- runQ $ createGraph graph
  putStrLn $ show result
 {- 
  let t = Create (toNode $ DoubleSmth 1)
  let e = Create (toNode $ DoubleSmth 2)

  let r1 = toURelation $ THIS_IS_RELATION 20
  let r2 = toURelation $ THIS_IS_RELATION 40
  let r3 = toURelation $ THIS_IS_ANOTHER_RELATION
  let nodes = fromList [("f", f), ("s", s), ("t", t), ("e", e)]
  let rels = fromList [(("f", "s"), r1), (("s", "t"), r2), (("f", "t"), r2), (("s", "e"), r3)]
  let graph = Graph nodes rels
  result <- runQ $ createGraph graph
  putStrLn $ show result


  let smth1Id = (_vertices result) M.! "f"
  let smth2Id = (_vertices result) M.! "s"
  let dsmth1Id = (_vertices result) M.! "t"
  let dsmth2Id = (_vertices result) M.! "e"
  let sel1 = NodeSelector (Just smth1Id) Nothing
  let sel2 = NodeSelector (Just smth2Id) Nothing
  let sel3 = NodeSelector (Just dsmth1Id) Nothing
  let sel4 = NodeSelector (Just dsmth2Id) Nothing
--
  let selector11 = URelSelector Nothing
  let selector12 = URelSelector Nothing
  let selector13 = URelSelector Nothing
--
  let graphSel = Graph (fromList [("f", sel1), ("s", sel2), ("t", sel3), ("e", sel4)])
                                    (fromList [(("f", "s"), selector11), (("s", "t"), selector12), (("f", "t"), selector12), (("s", "e"), selector13)])
  result <- runQ $ getGraph graphSel

  putStrLn $ show result


  let a :: Smth = fromNode $ ((_vertices result) M.! "f")
  let b :: Smth = fromNode $ ((_vertices result) M.! "s")
  let c :: DoubleSmth = fromNode $ ((_vertices result) M.! "t")
  let d :: DoubleSmth = fromNode $ ((_vertices result) M.! "e")
  let aa :: THIS_IS_RELATION = fromURelation $ ((_relations result) M.! ("f", "s"))
  let bb :: THIS_IS_RELATION = fromURelation $ ((_relations result) M.! ("s", "t"))
  let cc :: THIS_IS_RELATION = fromURelation $ ((_relations result) M.! ("f", "t"))
  let dd :: THIS_IS_ANOTHER_RELATION = fromURelation $ ((_relations result) M.! ("s", "e"))


  putStrLn $ show result

  putStrLn $ show a
  putStrLn $ show b
  putStrLn $ show c
  putStrLn $ show d

  putStrLn $ show aa
  putStrLn $ show bb
  putStrLn $ show cc
  putStrLn $ show dd
-}
--
  --let rels = [(0, 1), (1, 2), (0, 2), (1, 3)]
  --let graph = BoltGraphU [f, s, t, e] [r1, r2, r2, r3] rels
  --result <- runQ $ createGraph graph
  ----putStrLn $ show result
  --let smth1Id = getBoltId $ (vertices result) !! 0
  --let smth2Id = getBoltId $ (vertices result) !! 1
  --let dsmth1Id = getBoltId $ (vertices result) !! 2
  --let dsmth2Id = getBoltId $ (vertices result) !! 3
--
  ----let r1 = toURelation $ THIS_IS_RELATION 20
  ----let r2 = toURelation $ THIS_IS_RELATION 40
  ----let r3 = toURelation $ THIS_IS_ANOTHER_RELATION
  ----
  ----relation1 <- runQ $ createRelationship smth1Id r1 smth2Id
  ----relation2 <- runQ $ createRelationship smth2Id r2 dsmth1Id
  ----relation3 <- runQ $ createRelationship smth1Id r2 dsmth1Id
  ----relation4 <- runQ $ createRelationship smth2Id r3 dsmth2Id
----
--
  --putStrLn $ show result
--
  --putStrLn $ show a
  --putStrLn $ show b
  --putStrLn $ show c
  --putStrLn $ show d
--
  --putStrLn $ show aa
  --putStrLn $ show bb
  --putStrLn $ show cc
  --putStrLn $ show dd
  -- initialise Simple node and merge it into database.
  --let simple = Node (-1) ["Simple"] $ fromList [("fieldS", T "This is Text")]
  --simples <- runQ $ mergeNode simple
  --putStrLn . printf "Simple node result: %s" $ show simples

  -- initialize another "bigger" node and put it into database.
  --let bigger = Node (-1) ["Bigger"] $ fromList [("fieldD", F 42.0), ("fieldL", L [I 1, I 2])]
  --biggers <- runQ $ mergeNode bigger
  --putStrLn . printf "Bigger node result: %s" $ show biggers

  -- suppose that in one Node you want to store information from different nodes.
  -- Thus we can union information from different nodes and also load into database.
  --let unioned = simple `union` bigger
  --unioneds <- runQ $ mergeNode unioned
  --putStrLn . printf "Unioned node result: %s" $ show unioneds

  -- let select some nodes.
  --let simpleId = getBoltId $ head simples
  --let biggerId = getBoltId $ head biggers
  --let unionedId = getBoltId $ head unioneds

  -- we can load relation only if nodes are loaded already and have "adequate" IDs.
  --let relation = URelationship (-1) "THIS_IS_RELATION" $ fromList [("relFieldB", B True)]
  --relation' <- runQ $ createRelationship simpleId relation biggerId
  --putStrLn . printf "Relation result: %s" $ show relation'

  --let relation1 = URelationship (-1) "THIS_IS_RELATION" $ fromList [("relFieldB", B False)]
  --relation1' <- runQ $ createRelationship simpleId relation1 unionedId
  --putStrLn . printf "Relation result: %s" $ show relation1'

  --let selector1 = NodeSelector (Just simpleId) "a" Nothing
  --let selector2 = NodeSelector (Just biggerId) "b" Nothing
  --let selector5 = NodeSelector (Just unionedId) "c" Nothing
  --let selector3 = RelGraphSelector (Just 0) (Just 1) "r" (Just "THIS_IS_RELATION")
  --let selector4 = RelGraphSelector (Just 0) (Just 2) "rr" (Just "THIS_IS_RELATION")
  --let graphSel = GraphSelector [selector1, selector2] [selector3]
  --result <- runQ $ getGraph graphSel
  --putStrLn $ show result
  --let selector1 = RelSelector (Just 0) (Just 1) (Just "THIS_IS_RELATION")
  --let selector3 = RelSelector (Just 0) (Just 2) (Just "THIS_IS_RELATION")
  --let graphSel = GraphSelector [selector2, selector] [selector1, selector3]
  --result <- runQ $ getGraph graphSel
  --putStrLn $ show result
  --putStrLn $ show (length (_vertices result))
  --putStrLn $ show (length (_edges result))

  --let selector1 = RelSelector Nothing Nothing Nothing (Just "THIS_IS_RELATION")
  --result1 <- runQ $ getRelationships selector1
  --putStrLn $ show result1
  --result2 <- runQ $ getRelationships selector3
  --putStrLn $ show result2


