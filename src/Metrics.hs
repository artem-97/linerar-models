module Metrics where

a = [1..50] :: [Double]
b = map (subtract 2 . (* 5)) a
c = [1..3]

x = [1..100] :: [Double]
y = [1..100] :: [Double]

-- Regression
mse :: [Double] -> [Double] -> Maybe Double
mse xs ys
    | length xs /= length ys = Nothing
    | otherwise = Just mean
    where 
        all = sum $ map (^2) $ zipWith (-) xs ys
        mean = (all/) $ fromIntegral $ length xs    

mae :: [Double] -> [Double] -> Maybe Double
mae xs ys
    | length xs /= length ys = Nothing
    | otherwise = Just mean
    where 
        all = sum $ map abs $ zipWith (-) xs ys
        mean = (all/) $ fromIntegral $ length xs     

-- Classification
xcl = [1,0,0,0,1,1,0,1,0] :: [Double]
xcl' = [0.3,0.4,0.6,0,1,1,0,1,0] :: [Double]
ycl = [1,0,0,0,1,1,0,1,0] :: [Double]

accuracyScore ::[Double] -> [Double] -> Maybe Double
accuracyScore xs ys
    | length xs /= length ys = Nothing
    | otherwise = Just frac
    where
        same = sum $ zipWith (\x y -> fromEnum $ (==) x y) xs ys
        frac = (fromIntegral same/) $ (fromIntegral . length) xs 


logLoss :: [Double] -> [Double] -> Maybe Double
logLoss yTrue yPred
    | length yTrue /= length yPred = Nothing
    | otherwise = Just loss
    where
        filtered = [(y,p)| y <- yTrue, p <- yPred, 0 /= p && p/= 1]
        (ys,ps) = unzip filtered
        loss' n
            | n == 0 = 0 :: Double
            | otherwise = sum $ zipWith (\y p -> y * log p + (1-y) * log (1-p)) ys ps
        loss = loss' $ length ys





