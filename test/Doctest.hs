module Main where

import           Test.DocTest (doctest)

-- Taken from https://github.com/kowainik/membrain/blob/master/test/Doctest.hs

main :: IO ()
main =
  doctest
    [ "-isrc"
    , "src/Database/Bolt/Extras/DSL/Typed.hs"
    , "src/Database/Bolt/Extras/DSL/Typed/Types.hs"
    ]
