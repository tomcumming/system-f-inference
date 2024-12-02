module Main (main) where

import Test.Tasty (defaultMain)
import Tests.IPF (ipfTests)

main :: IO ()
main = defaultMain ipfTests
