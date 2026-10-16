module Main (main) where

import Test.Tasty (defaultMain)
import qualified V2Test

main :: IO ()
main = defaultMain V2Test.tests
