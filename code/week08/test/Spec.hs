module Main
    ( main
    ) where

<<<<<<< HEAD
import qualified Spec.Model
import qualified Spec.ModelWithClose
import qualified Spec.Trace
import qualified Spec.TraceWithClose
=======
import qualified Spec.ModelClosable
import qualified Spec.TraceClosable
>>>>>>> homework week 8
import           Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
<<<<<<< HEAD
tests = testGroup "token sale"
    [ Spec.Trace.tests
    , Spec.TraceWithClose.tests
    , Spec.Model.tests
    , Spec.ModelWithClose.tests
    ]
=======
tests = testGroup "token sale closable"
    [ Spec.TraceClosable.tests
    , Spec.TraceClosable.tests2
    , Spec.ModelClosable.tests
    ]
>>>>>>> homework week 8
