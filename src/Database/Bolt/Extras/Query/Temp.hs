{-# LANGUAGE OverloadedStrings #-}

module Database.Bolt.Extras.Query.Temp where

-- @def@ from package data-default
import           Data.Default               (def)
import           Data.Map.Strict            (Map, fromList)
import           Database.Bolt
import           Database.Bolt.Extras.Query
import           Database.Bolt.Extras.Utils
import           Text.Printf                (printf)

-- | this function just run prepared query and return answer
runQ :: BoltActionT IO a -> IO a
runQ action = do
    pipe   <- connect boltCfg
    result <- run pipe action
    close pipe
    pure result
  where
    boltCfg = def { user     = "neo4j"     -- database login, neo4h by default
                  , password = "lvbm123"   -- passwird for database
                  , host     = "localhost" -- host with database
                  , port     = 7687        -- standart port
                  }


runWithoutClass :: IO ()
runWithoutClass = do
  -- initialise Simple node and merge it into database.
  let simple = Node (-1) ["Simple"] $ fromList [("fieldS", T "This is Text")]
  simples <- runQ $ mergeNode simple
  putStrLn . printf "Simple node result: %s" $ show simples

  -- initialize another "bigger" node and put it into database.
  let bigger = Node (-1) ["Bigger"] $ fromList [("fieldD", F 42.0), ("fieldL", L [I 1, I 2])]
  biggers <- runQ $ mergeNode bigger
  putStrLn . printf "Bigger node result: %s" $ show biggers

  -- suppose that in one Node you want to store information from different nodes.
  -- Thus we can union information from different nodes and also load into database.
  let unioned = simple `union` bigger
  unioneds <- runQ $ mergeNode unioned
  putStrLn . printf "Unioned node result: %s" $ show unioneds

  -- let select some nodes.
  let simple' = head simples
  let bigger' = head biggers

  -- we can load relation only if nodes are loaded already and have "adequate" IDs.
  let relation = URelationship (-1) "THIS_IS_RELATION" $ fromList [("relFieldB", B True)]
  relation' <- runQ $ mergeRelationship simple' bigger' relation
  putStrLn . printf "Relation result: %s" $ show relation'


