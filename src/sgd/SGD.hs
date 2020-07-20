module SGD where

import qualified Numeric.AD      (diff, diff')
import qualified Numeric.Algebra


diff :: (Fractional a) => a -> (a -> a) -> (a -> a)
diff h f x = (f (x+h) - f(x)) /2

--gdStep :: (a -> a) -> a -> a
--gdStep f x = 0.001 * Numeric.AD.diff f(x)

main = do putStrLn $ show $ diff 0.001 exp(1)
