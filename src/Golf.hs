module Golf
    ( skips
    , localMaxima
    , histogram
    ) where

import Data.List (maximumBy, transpose)
import Data.Ord (comparing)

skips :: (Eq a, Show a) => [a] -> [[a]]
skips = go 1
  where
    go n xs = case every n xs of
        [] -> []
        xs' -> xs' : go (n + 1) xs

    every :: Int -> [a] -> [a]
    every n = concat . zipWith (\i x -> [x|i `mod` n == 0]) [1..]

localMaxima :: [Integer] -> [Integer]
localMaxima (x:(rest@(y:z:_)))
    | y > x && y > z = y : localMaxima rest
    | otherwise = localMaxima rest
localMaxima _ = []

histogram :: [Int] -> String
histogram xs = unlines $ transpose $ map column hs
  where
    column :: (Int, Int) -> String
    column (n, h) = concat
        [ replicate (maxH - h) ' '
        , replicate h '*', "=", show n
        ]

    hs :: [(Int, Int)]
    hs = map (\n -> (n, length $ filter (== n) xs)) [0..9]

    maxH :: Int
    maxH = snd $ maximumBy (comparing snd) hs
