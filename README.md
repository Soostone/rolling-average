# Rolling Averages

`rolling-averages` is a simple library for calculating rolling
avergaes. It keeps a rolling window of samples and calculates averages
from them.

Example:

```haskell
import Control.Monad.State
import Statistics.RollingAverage
import System.Random

makeAvg :: IO Int
makeAvg = flip evalStateT (mkRavg windowSize) $ do
  replicateM_ 20 $ do
    n <- liftIO randomIO
    modify (\s -> ravgAdd s n)
  where
    windowSize = 10
```
