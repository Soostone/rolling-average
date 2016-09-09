module Main
    ( main
    ) where


-------------------------------------------------------------------------------
import           Criterion
import           Criterion.Main
import           Data.Foldable
-------------------------------------------------------------------------------
import           Statistics.RollingAverage
-------------------------------------------------------------------------------


main :: IO ()
main = defaultMain [
  bgroup "ravgAdd" $ flip map [1, 100, 1000] $ \n ->
    bgroup (show n) [
        bench "Int" $ whnf (foldl' ravgAdd (mkRavg n)) ([1..fromIntegral n] :: [Int])
      , bench "Integer" $ whnf (foldl' ravgAdd (mkRavg n)) ([1..fromIntegral n] :: [Integer])
      ]
  , bgroup "ravg" $ flip map [1, 100, 1000] $ \n ->
      bgroup (show n) [
          bench "Int" $ whnf ravg (foldl' ravgAdd (mkRavg n) ([1..fromIntegral n] :: [Int]))
        , bench "Integer" $ whnf ravg (foldl' ravgAdd (mkRavg n) ([1..fromIntegral n] :: [Integer]))
      ]
  ]


-------------------------------------------------------------------------------
avgList :: Num a => Int -> [a] -> RollingAvg a
avgList lim samples = foldl' ravgAdd (mkRavg lim) samples
