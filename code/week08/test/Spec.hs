module Main
    ( main
    ) where


import qualified Spec.Model
import qualified Spec.ModelWithClose
import qualified Spec.Trace
import qualified Spec.TraceWithClose
import           Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "token sale"
    [ Spec.Trace.tests
    , Spec.TraceWithClose.tests
    , Spec.Model.tests
    , Spec.ModelWithClose.tests
    ]

-- tests = testGroup "token sale closable"
--     [ Spec.TraceClosable.tests
--     , Spec.TraceClosable.tests2
--     , Spec.ModelClosable.tests
