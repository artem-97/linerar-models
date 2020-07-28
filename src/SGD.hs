module SGD where
import           Data.Optional

eps = 1e-5 :: Double
eta = 1e-3 :: Double

diff :: (Double -> Double) -> Double ->Double
diff f x = (f (x + eps) - f (x - eps) )/ (2 * eps)


gdStep :: (Double -> Double) -> Double -> Double
gdStep f x = x - eta * diff f x


gdFindMin' :: (Double ->Double) -> Double -> Integer -> Double
gdFindMin' f x n
      | n <= 0    = x
      | otherwise = gdFindMin'  f (gdStep f x) (n-1)


gdFindMin :: (Double ->Double) -> Double -> Double
gdFindMin f x = gdFindMin' f x 10000
