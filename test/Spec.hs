import Test.Hspec.Expectations
import Test.Hspec.Core.Formatters (writeLine)
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import System.CPUTime (getCPUTime)
import Text.Printf
import Data.List
import System.Environment
import System.Random
import Quicksort

main :: IO ()
main = defaultTestFunc
-- do creates a chain of (>>) and (>>=) expressions
defaultTestFunc = do
    runHspecUnitTests

runHspecUnitTests = hspec $ do
    describe "Test basic functionality of Quicksort" $ do
        it "Sorts a basic reverse ordered list [5, 4, 3, 2, 1]" $ do
            quicksort [5, 4, 3, 2, 1] `shouldMatchList` [1, 2, 3, 4, 5]

        it "Sorts a more complex list of [10, 5, 9, 4, 8, 3, 7, 2, 6, 1]" $ do
            quicksort [10, 5, 9, 4, 8, 3, 7, 2, 6, 1] `shouldMatchList` [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

        it "Quicksort diagnostics done" $ do
            runSortingDiagnostics quicksort

    describe "Test basic functionality of Parallel Quicksort" $ do
        it "Sorts a basic reverse ordered list [5, 4, 3, 2, 1]" $ do
            paraQuicksort [5, 4, 3, 2, 1] `shouldMatchList` [1, 2, 3, 4, 5]

        it "Sorts a more complex list of [10, 5, 9, 4, 8, 3, 7, 2, 6, 1]" $ do
            paraQuicksort [10, 5, 9, 4, 8, 3, 7, 2, 6, 1] `shouldMatchList` [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

        it "Quicksort diagnostics done" $ do
            runSortingDiagnostics paraQuicksort

runSortingDiagnostics f = do
    printf "\n\nRunning Quicksort Diagnostics..."
    pseudoRandomInput f (mkStdGen 200)

pseudoRandomInput f g = do
    let l = take 1000000 (randoms g :: [Int])
    time $ f l `seq` return ()

-- "time" is taken from https://wiki.haskell.org/Timing_computations
time :: IO t -> IO t
time a = do
    start <- getCPUTime
    v <- a
    end   <- getCPUTime
    let diff = (fromIntegral (end - start)) / (10^12)
    printf "\n\tComputation time: %0.3f sec\n" (diff :: Double)
    return v
