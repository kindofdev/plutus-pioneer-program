module Main
    ( main
    ) where

<<<<<<< HEAD
<<<<<<< HEAD
import qualified Spec.Model
import qualified Spec.ModelWithClose
import qualified Spec.Trace
import qualified Spec.TraceWithClose
=======
import qualified Spec.ModelClosable
import qualified Spec.TraceClosable
>>>>>>> homework week 8
=======
import qualified Spec.ModelClosable
import qualified Spec.TraceClosable
>>>>>>> 7f8d7c0964caee9a56cf1ff40d0e5b8d7a74719f
import           Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
<<<<<<< HEAD
<<<<<<< HEAD
tests = testGroup "token sale"
    [ Spec.Trace.tests
    , Spec.TraceWithClose.tests
    , Spec.Model.tests
    , Spec.ModelWithClose.tests
    ]
=======
=======
>>>>>>> 7f8d7c0964caee9a56cf1ff40d0e5b8d7a74719f
tests = testGroup "token sale closable"
    [ Spec.TraceClosable.tests
    , Spec.TraceClosable.tests2
    , Spec.ModelClosable.tests
<<<<<<< HEAD
    ]
>>>>>>> homework week 8
=======
    ]
>>>>>>> 7f8d7c0964caee9a56cf1ff40d0e5b8d7a74719f
