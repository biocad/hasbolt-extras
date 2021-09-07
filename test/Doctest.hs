module Main where

import           Test.DocTest (doctest)

-- Taken from https://github.com/kowainik/membrain/blob/master/test/Doctest.hs

main :: IO ()
main = do
  doctest
    [ "-isrc"
    , "src/Database/Bolt/Extras/DSL/Typed.hs"
    , "src/Database/Bolt/Extras/DSL/Typed/Types.hs"
    , "src/Database/Bolt/Extras/DSL/Typed/Parameters.hs"
    , "src/Database/Bolt/Extras/Generic.hs"
    ]
  --  This has to be run separately due to some complications with TH and/or internal modules
  --  See here: https://github.com/sol/doctest/issues/160
  doctest
    [ "-isrc"
    , "src/Database/Bolt/Extras/Template/Internal/Converters.hs"
    ]
