{-# LANGUAGE RecordWildCards #-}
module Statistics.RollingAverage
    ( RollingAvg
    , ravgWindowSize
    , ravgSamples
    , ravg
    , ravgAdd
    , mkRavg
    ) where


-------------------------------------------------------------------------------
import           Data.Maybe    (fromMaybe)
import qualified Data.Sequence as Seq
-------------------------------------------------------------------------------


-- | RollingAvg is a rolling avg with the following properties:
--
-- 1. It has a fixed window size. Samples are shufled through in first-in-first-out (FIFO) order.
--
-- 2. It pre-computes the sum of the average and uses an O(1)
-- operation for calculating the count to calculate the
-- average. Taking the average is O(1).
--
-- 3. This is not an "online" average, meaning that the samples do
-- need to be held in memory. At a future date it may be a good idea
-- to add an online variant for those with extremely large sample
-- windows or tight memory constraints.
--
-- 4. The sample type is effectively (Num a, Integral a) => a. This
-- lets you use Integer or perhaps more efficient types if you don't
-- need arbitrarily-sized integers.
data RollingAvg a = RollingAvg {
      ravgWindowSize :: !Int
    , ravgSamples    :: !(Seq.Seq a)
    , ravgSum        :: !a
    } deriving (Show)


-------------------------------------------------------------------------------
mkRavg :: (Num a) => Int -> RollingAvg a
mkRavg windowSize = RollingAvg windowSize Seq.empty 0


-------------------------------------------------------------------------------
ravg :: (Integral a, Fractional b) => RollingAvg a -> b
ravg RollingAvg {..}
  | samples == 0 = 0.0
  | otherwise    = fromIntegral ravgSum / fromIntegral samples
  where
    samples = Seq.length ravgSamples


-------------------------------------------------------------------------------
ravgAdd :: (Num a) => RollingAvg a -> a -> RollingAvg a
ravgAdd ra@RollingAvg {..} x =
  ra { ravgSamples = Seq.take ravgWindowSize (x Seq.<| ravgSamples)
     , ravgSum = sum'
     }
  where
    willTruncate = Seq.length ravgSamples == ravgWindowSize
    sum'
      | willTruncate = ravgSum + x - oldest
      | otherwise = ravgSum + x
    oldest = fromMaybe 0 (slast ravgSamples)


-------------------------------------------------------------------------------
slast :: Seq.Seq a -> Maybe a
slast s = case Seq.viewr s of
  _ Seq.:> a -> Just a
  _          -> Nothing
