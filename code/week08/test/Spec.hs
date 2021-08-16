module Main
    ( main
    ) where

import qualified Spec.ModelClosable
import qualified Spec.TraceClosable
import           Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "token sale closable"
    [ Spec.TraceClosable.tests
    , Spec.TraceClosable.tests2
    , Spec.ModelClosable.tests
    ]