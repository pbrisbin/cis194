module Golf
    ( skips
    , localMaxima
    , histogram
    ) where

import Data.List (group, sort, transpose)

-- | Return [every, every 2nd, ..., every nth] element in a list
--
-- 1. Get every nth element in the list use zip and list comprehension
--   - zipWith [1..] gives us indexed pairs of the source list
--   - 1-based indexes preserve the conceptual meaning of "nth"
--   - [x|...] is [x] if the condition's true
--   - [x|...] is [] otherwise
--   - ev naturally returns [] when n grows too large
-- 3. If ev returns a non-empty, cons it with recursion on n+1
--
skips :: (Eq a, Show a) => [a] -> [[a]]
skips = go 1
  where
    go n xs = case ev n xs of
        [] -> []
        xs' -> xs' : go (n + 1) xs

    -- every n'th element in the list
    ev :: Int -> [a] -> [a]
    ev n = concat . zipWith (\i x -> [x|i `mod` n == 0]) [1..]

-- | Return every element greater than both its neighbors
--
-- 1. Pattern match sets of three elements
-- 2. Store non-first element in an as-pattern
-- 3. Use list comprehension:
--   - [y|...] evaluates to [y] if the condition is true
--   - [y|...] evaluates to [] otherwise
-- 4. Prepend [y] (or []) onto a resursive call with (r)est
--
localMaxima :: [Integer] -> [Integer]
localMaxima (x:(r@(y:z:_))) = [y|y > x, y > z] ++ localMaxima r
localMaxima _ = []

-- | Format columns representing frequency of numbers in the input list
--
-- 1. Take numbers 0-9 (known, fixed domain)
-- 2. Map them directly to a formatted column
-- 3. Transpose that to rows
-- 4. Join rows with newlines
--
histogram :: [Int] -> String
histogram xs = unlines $ transpose $ map c [0..9]
  where
    -- column: [(max - height) spaces, height *'s, an =, then the number]
    c n = concat
        [ replicate (m - h n) ' '
        , replicate (h n) '*', "=", show n
        ]

    -- height: the number of ns in xs
    h n = length $ filter (== n) xs

    -- max height: count of most frequent number
    m = maximum $ map length $ group $ sort xs
