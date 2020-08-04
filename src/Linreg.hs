module Linreg where

import System.Random
import Control.Monad
import           SGD

a = [1..50] :: [Double]
b = map (subtract 2 . (* 5)) a

x = [1..10] :: [Double]
y = reverse [-10..(-1)] :: [Double]


linReg2d :: [Double] -> [Double] -> Double -> Double -> Integer -> (Double, Double)
linReg2d x y w b epochs
    | epochs == 0 = (w,b)
    | otherwise = linReg2d x y newW newB (epochs - 1)
    where
        n = fromIntegral $ length x
        -- d/dw(Loss) = -2 * x_i * (y_i - (w * x_i + b))
        dw' = map ((+b) . (*w)) x
        dw'' = sum $ zipWith (*) x (zipWith (-) y dw')
        dw = (-2) *  dw''

        -- d/db(Loss) = -2 * (y_i - (w * x_i + b))
        db' = map ((+b) . (*w)) x
        db'' = sum $ zipWith (-) y db'
        db = (-2) * db''

        newW = w - eta * dw / n
        newB = b - eta * db / n