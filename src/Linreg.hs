module Linreg where

import           SGD

a = [1..100] :: [Double]
b = map (+2)$ map (*5) a


{-
    Y_pred = m*X + c  # The current predicted value of Y
    D_m = (-2/n) * sum(X * (Y - Y_pred))  # Derivative wrt m
    D_c = (-2/n) * sum(Y - Y_pred)  # Derivative wrt c
    m = m - L * D_m  # Update m
    c = c - L * D_c  # Update c
 - -}

linReg :: [Double] -> [Double] -> Double -> Double -> Integer -> (Double, Double)
linReg x y m c epochs
    | epochs == 0 =  (m,c)
    | otherwise = linReg  x y newM newC (epochs - 1)
    where
        yPred = map (+c) $ map (*m) x
        dy = zipWith (-) y yPred
        dm = - (sum (zipWith (*) x dy))
        dc = - (sum dy)
        newM = m - eta * dm
        newC = c - eta * dc
