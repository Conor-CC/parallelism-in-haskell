import Test.Hspec
import Test.Hspec.Expectations
import Test.QuickCheck
import Control.Exception (evaluate)
import Test.Hspec.Core.Formatters
import Quicksort

main :: IO ()
main = hspec $ do
    describe "Quicksort_Basics" $ do
        it "Test_Simple_List" $ do
            let l = [10, 9, 8, 7, 6, 5]
            let t1 = getCPUTime
            quicksort l `shouldMatchList` [5, 6, 7, 8, 9, 10]

        it "Test_Different_Simple_List" $ do
            let l = [10, 9, 8, 7, 6, 5]
            let t1 = getCPUTime
            quicksort l `shouldMatchList` [5, 6, 7, 8, 9, 10]
