{-# LANGUAGE ScopedTypeVariables #-}
module Main
    ( main
    ) where


-------------------------------------------------------------------------------
import           Data.Foldable             as FT
import           Data.Ratio
import           Data.Sequence             as Seq
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck
-------------------------------------------------------------------------------
import           Statistics.RollingAverage
-------------------------------------------------------------------------------


main :: IO ()
main = defaultMain tests


tests :: TestTree
tests = testGroup "rolling-average"
  [
    testProperty "window size of 0 always produces 0" $ \(samples :: [Int]) ->
      ravg (avgList 0 samples) === (0.0 :: Double)
  , testProperty "never grows samples larger than the limit" $ \(Positive lim) (samples :: [Int]) ->
      Seq.length (ravgSamples (avgList lim samples)) <= lim
  , testCase "can calculate rolling average as a Ratio" $ do
      ravg (avgList 3 ([0, 1, 2, 3, 4, 5] :: [Int])) @?= (((3 + 4 + 5) % 3) :: Rational)
  ]


-------------------------------------------------------------------------------
avgList :: Num a => Int -> [a] -> RollingAvg a
avgList lim samples = FT.foldl' ravgAdd (mkRavg lim) samples
